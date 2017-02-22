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
  o <- lapply(tau, function(x) {
    do.call("rq", list(formula=as.formula(formula), tau=x, data=substitute(data), ...))
  })
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
  }, ...)
  names(p)[1] <- "tau"
  p$tau <- factor(p$tau)
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
# # basic test of rql, summary, and prediction
# m <- rql(foodexp ~ income, data=engel, tau=c(.25, .5, .75))
# summary(m)
# head(predict(m, newdata=engel))
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
# @param .parallel, .progress passed to ldply()
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
    }, ...)
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
# rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, interval="confidence", se="boot")
# rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, interval="confidence", se="boot", .parallel=TRUE)
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
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit) +
#   ylim(-200, 100)


## Quantile-based ANOVA ----

# aov()-like function based on quantile regression
#
# @param formula a formula object, like for aov()
# @param data a data.frame in which to interpret the variables named in the formula
# @param tau the quantile(s) to be estimated
# @param ... passed to anova.rq
aovq <- function(formula, data, tau, test="anowar", R=1000, ...) {
  ans <- plyr::llply(tau, function(tau) {
    # compute regression and null model for this quantile
    suppressWarnings(m <- rq(formula, tau=tau, data=data))
    suppressWarnings(mnull <- rq(update(formula, . ~ 1), tau=tau, data=data))
    # compare them
    suppressWarnings(a <- anova(mnull, m, test=test, R=R, ...))

    # NB: warnings are suppressed because, with a categorical explanatory variable, it is very common that the median is not exactly defined (when the number of observations is even for example) and this yields a warning of the form "In rq.fit.br(x, y, tau = tau, ...) : Solution may be nonunique". This is not ideal because other warnings may be relevant but this was too annoying to be left alone.

    # identify the quantile (as the first column)
    a$table$tau <- tau
    a$table <- a$table[,c(5,1:4)]
    return(a)
  })
  # combine ANOVA tables for all quantiles
  ans[[1]]$table <- ldply(ans, `[[`, "table")
  # keep only the combined version
  ans <- ans[[1]]
  # give it a new class to handle printing it
  class(ans) <- "anova.rqs"
  return(ans)
}

# Printing method for results of aovq
#
# identical to print.anova.rq except for the added tau column
print.anova.rqs <- function (x, ...){
  table <- x$table
  topnote <- x$topnote
  dimnames(table)[[2]] <- c("tau", "Df", "Resid Df", "F value", "Pr(>F)")
  title <- "Quantile Regression Analysis of Deviance Table\n"
  a <- structure(table, heading = c(title, topnote), class = c("anova", "data.frame"))
  print(a)
}
