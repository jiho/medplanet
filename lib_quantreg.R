#
# Utility functions for quantile regression
#
# non-parametric regression functions are inspired by vignette("rq")
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("quantreg")
library("logspline")

# library("tidyverse") # for some examples

## Use multiple quantiles in a list ----

# Fit quantile regression separately for each tau
#
# This is useful to each prediction of confidence intervals per tau (which is currently impossible for predict.rq with multiple tau)
# @param the usual arguments of rq
rql <- function(formula, tau, data, ...) {
  my_rq <- function(tau, formula, data, ...) {do.call("rq", list(formula=as.formula(formula), tau=tau, data=substitute(data), ...))}
  o <- do.call("lapply", list(X=tau, FUN=my_rq, formula=as.formula(formula), data=substitute(data), ...))
  # NB: based on http://stackoverflow.com/questions/14933804/trouble-passing-on-an-argument-to-function-within-own-function
  #     using do.call evaluates the arguments and therefore avoids most scoping issues
  class(o) <- c("rql", class(o))
  names(o) <- tau
  return(o)
}

# Summary method for quantile regression list
#
# @param object list of `rq` objects, returned by rql()
# @param ... passed to summary.rq
summary.rql <- function(object, ...) {
  o <- lapply(object, summary, ...)
  names(o) <- names(object)
  return(o)
}

# Quantile regression prediction from list
#
# @param object list of rq() objects, returned by rql
# @param ... passed to predict.rq (and ldply => .parallel can be used)
# NB: always returns a data.frame with at least columns tau (factor) and fit
predict.rql <- function(object, ...) {
  p <- plyr::ldply(object, function(x, ...) {
    p <- predict.rq(x, ...)
    if (is.vector(p)) {
      p <- data.frame(fit=p)
    } else {
      p <- as.data.frame(p)
    }
    return(p)
  }, .id="tau", ...)
  return(p)
}

# Transform an rqs object into an rql object
#
# @param object an rqs object, output by rq when tau is a vector
as.rql <- function(object, ...) {
  if (!"rqs" %in% class(object)) {
    stop("Need an object of class rqs")
  }
  l <- list()
  for (i in 1:length(object$tau)) {
    x <- object
    # subset everything to make the content similar to the output of rq
    x$coefficients <- x$coefficients[,i]
    x$fitted.values <- x$fitted.values[,i]
    names(x$fitted.values) <- 1:length(x$fitted.values)
    x$residuals <- x$residuals[,i]
    x$tau <- x$tau[i]
    x$call[3] <- x$tau
    x$rho <- x$rho[i]
    # remove dual because we can't make it similar to rq
    x$dual <- NULL
    # assign class
    class(x) <- "rq"
    l[[as.character(x$tau)]] <- x
  }
  class(l) <- "rql"
  return(l)
}


# data("engel")
# # basic test of rql, summary, and, model modification, and prediction
# m <- rql(foodexp ~ income, data=engel, tau=c(.25, .5, .75))
# summary(m)
# head(predict(m, newdata=engel))
# update(m[[1]], .~1)
# head(p <- predict(m, newdata=engel, int="conf"))
# p$income <- engel$income
# ggplot() +
#   geom_point(aes(income, foodexp), data=engel) +
#   geom_ribbon(aes(income, ymin=lower, ymax=higher, fill=tau), data=p, alpha=0.5) +
#   geom_path(aes(income, fit, colour=tau), data=p)
#
# # parallel computation
# new <- data.frame(income=runif(5000, 200, 4900))
# system.time(p <- predict(m, newdata=new, int="conf"))
# library("doParallel")
# registerDoParallel(cores=2)
# system.time(p <- predict(m, newdata=new, int="conf", .parallel=TRUE))
# stopImplicitCluster()
# # -> works, twice as fast!
#
# # convert between rqs qnd rql
# m <- rq(foodexp ~ income, data=engel, tau=c(.25, .5, .75))
# head(predict(m))
# head(predict(as.rql(m)))
# # test the conversion gives exactly similar objects
# m1ref <- rq(foodexp ~ income, data=engel, tau=0.25)
# m1 <- as.rql(m)[[1]]
# for (n in names(m1ref)) {
#   message(n, " ", identical(m1[[n]], m1_ref[[n]]))
# }
# # -> ok for all but dual (as expected)


## Tools ----

# Smooth data using a moving average
#
# @param x vector, matrix, or data.frame of data
# @param order order of the moving average, will be made integer. The window is of size 2*k+1
mav <- function(x, k, ...) {
  if (is.vector(x)) {
    x <- matrix(x, ncol=1)
  }

  # prepare the weights for filtering
  k <- round(k)
  w <- c(seq(1, k, by=1), k+1, seq(k, 1, by=-1))
  w <- w/sum(w)

  # padd the data at the beginning and end to avoid loosing data
  n <- nrow(x)
  x_padded <- x[c(rep(1, times=k), 1:n, rep(n, times=k)), , drop=F]

  # pass the moving average
  xf_padded <- stats::filter(x_padded, w)

  # remove padding
  xf <- xf_padded[1:n+k,]

  return(as.data.frame(xf))
}


## Non parametric quantile regression ----

# Locally linear non-parametric quantile regression
#
# @param x univariate explanatory variable
# @param y univariate response variable
# @param tau quantile(s)
# @param bw bandwidth, i.e. scale of the fit (larger means smoother)
# @param smooth when > 0, smooth result using a moving average of order `smooth`
# @param .progress passed to the internal ldply() loop on n
# @param ... passed to `predict.rq` (and ldply => can use .parallel, etc.)
llrq <- function(x, y, tau=.5, bw=diff(range(x))/10, n=50, smooth=0, ...) {
  # create the vector of output points
  xx <- seq(min(x), max(x), length.out=n)

  # for each tau
  # NB: it is faster to split by tau than to use rql inside (i.e. fit for each tau separately inside)
  #     we can't fit for all taus and then get the predictions + CI separately for each tau (fails)
  #     in addition, parallelising at this stage is the easiest and most efficient
  plyr::ldply(tau, function(tau, ...) {
    p <- plyr::ldply(xx, function(xx, ...) {
      # center on the current point and define normally distributed weights around it
      z <- x - xx
      wx <- dnorm(z/bw)
      if (all(wx == 0)) {
        stop("No points within one bandwidth. Increase bw.")
      }
      # compute quantile regression
      r <- rq(y ~ z, weights=wx, tau=tau)
      # and extract fitted values
      p <- predict(r, data.frame(z=0), ...)
      return(p)
    }, .progress=.progress, ...)
    # identify x values and quantile
    p$x <- xx
    p$tau <- factor(tau)
    # force first column to be called fit
    names(p)[1] <- "fit"
    if (smooth != 0) {
      # detect which columns to smooth (i.e. all those when present)
      cols <- intersect(names(p), c("fit", "lower", "upper"))
      # run a moving average
      p[cols] <- mav(p[cols], smooth)
    }
    return(p)
  }, ...)
}

# data("mcycle", package="MASS")
#
# # test local linear fitting with or without smoothing
# rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3)
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit)
# rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, smooth=2)
# last_plot() + geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit, linetype="dashed")
#
# # with different bw
# rq_fit <- plyr::ldply(c(3,5,10), function(bw) {
#   p <- llrq(mcycle$times, mcycle$accel, tau=0.5, bw=bw)
#   p$bw <- bw
#   return(p)
# })
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_line(aes(x=x, y=fit, colour=factor(bw)), data=rq_fit)
#
# # with confidence interval
# system.time(rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, n=100, interval="confidence", se="boot"))
# library("doParallel")
# registerDoParallel(cores=3)
# system.time(rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, n=100, interval="confidence", se="boot", .parallel=TRUE))
# stopImplicitCluster()
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_ribbon(aes(x=x, ymin=lower, ymax=higher, fill=tau), data=rq_fit, alpha=0.3) +
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit)


# Spline-based non-parametric quantile regression
#
# @param x univariate explanatory variable
# @param y univariate response variable
# @param tau quantile(s)
# @param df number of knots of the spline (less means a smoother fit)
# @param ... passed to `predict.rq`
srq <- function(x, y, tau=.5, df=15, ...) {
  # prepare spline data
  library("splines")
  xx <- bs(x, df=df)
  # fit quantile regression
  m <- rq(y ~ xx, tau=tau)
  # extract the fit (and potentially confidence interval) for all quantiles
  fit <- predict(as.rql(m), newdata=xx, ...)
  fit$x <- x
  return(fit)
}

# library("MASS")
# data("mcycle")
#
# rq_fit <- srq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), df=15)
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit)
#
# rq_fit <- srq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), df=15, interval="confidence")
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_ribbon(aes(x=x, ymin=lower, ymax=higher, fill=tau), data=rq_fit, alpha=0.3) +
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit)


## Quantile-based ANOVA ----

# Fit a multiple quantile regression models which will be used in an ANOVA setting
#
# @param ... passed to rql()
aovq <- function(...) {
  # TODO check that the explanatory variables are categorical
  suppressWarnings(o <- rql(...))
  # NB: warnings are suppressed because, with a categorical explanatory variable, it is very common that the median is not exactly defined (when the number of observations is even for example) and this yields a warning of the form "In rq.fit.br(x, y, tau = tau, ...) : Solution may be nonunique". This is not ideal because other warnings may be relevant but this was too annoying to be left alone.
  class(o) <- "aovq"
  return(o)
}

# Perform an analysis of variance test
#
# @param object an aovq object
# @param test test statistic to use (passed to anova.rq)
# @param R number of resampling replications for the anowar form of the test (passed to anova.rq)
# @param ... passed to anova.rq
summary.aovq <- function(object, test="anowar", R=1000, ...) {
  o <- plyr::llply(object, function(m, ...) {
    # compute the null model for this quantile
    suppressWarnings(mnull <- update(m, . ~ 1))
    # compare them
    suppressWarnings(a <- anova.rq(mnull, m, test=test, R=R, ...))
    return(a)
  }, ...)
  # combine ANOVA tables for all quantiles
  o[[1]]$table <- plyr::ldply(o, `[[`, "table", .id="tau")
  # keep only the combined version
  o <- o[[1]]
  # give it a new class to handle printing it
  class(o) <- "summary.aovq"
  return(o)
}

# Printing method for results of summary.aovq
#
# @param x object of class summary.aovq, returned by summary.aovq()
# = identical to print.anova.rq except for the added tau column
print.summary.aovq <- function (x, ...){
  table <- x$table
  topnote <- x$topnote
  dimnames(table)[[2]] <- c("tau", "Df", "Resid Df", "F value", "Pr(>F)")
  title <- "Quantile Regression Analysis of Deviance Table\n"
  a <- structure(table, heading = c(title, topnote), class = c("anova", "data.frame"))
  print(a)
}

# set.seed(1)
# n <- 30
# d <- data.frame(x1=rnorm(n, 1), x2=rnorm(n,1.5), x3=rnorm(n, 0.5))
# d <- gather(d, key="x", value="y")
# qplot(x, y, data=d)
#
# summary(aov(y ~ x, data=d))
# summary(aovq(y ~ x, tau=c(0.25, 0.5, 0.75), data=d))



## ggplot helpers ----

# Inspired by stat_boxplot
stat_quantiled <- function(mapping = NULL, data = NULL,
                           geom = "quantiled", position = "identity",
                           ...,
                           quantiles = c(0.25, 0.5, 0.75),
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatQuantileD,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      quantiles = quantiles,
      na.rm = na.rm,
      ...
    )
  )
}


StatQuantiled <- ggproto(
  "StatQuantiled", Stat,
  required_aes = c("x", "y"),
  non_missing_aes = "weight",

  setup_params = function(data, params) {
    params$width <- params$width %||% (resolution(data$x) * 0.75)

    if (is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
      warning(
        "Continuous x aesthetic -- did you forget aes(group=...)?",
        call. = FALSE)
    }

    params
  },

  compute_group = function(data, scales, width = NULL, na.rm = FALSE, quantiles = c(0.25, 0.5, 0.75)) {
    if (!is.null(data$weight)) {
      mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = quantiles)
      stats <- as.numeric(stats::coef(mod))
    } else {
      stats <- as.numeric(stats::quantile(data$y, quantiles))
    }
    names(stats) <- quantiles

    if (length(unique(data$x)) > 1)
      width <- diff(range(data$x)) * 0.9

    df <- as.data.frame(as.list(stats), check.names=FALSE)

    if (is.null(data$weight)) {
      n <- sum(!is.na(data$y))
    } else {
      # Sum up weights for non-NA positions of y and weight
      n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
    }

    df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
    df <- reshape2::melt(df, id.vars=c("x"), value.name = "y", variable.name="quantile")
    df
  }
)

# Inspired by geom_quantile
geom_quantiled <- function(mapping = NULL, data = NULL,
                          stat = "quantiled", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuantiled,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomQuantiled <- ggproto("GeomQuantileD", GeomPoint,
                        default_aes = defaults(
                          aes(shape = 15, colour = "#3366FF"),
                          GeomPoint$default_aes
                        )
)

