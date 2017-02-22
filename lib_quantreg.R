#
# Utility functions for quantile regression
#
# non-parametric regression functions are inspired by vignette("rq")
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("quantreg")
library("logspline")

## Tools ----

# Fit rq separately for each tau
#
# This is useful to each prediction of confidence intervals per tau (which is currently impossible for predict.rq with multiple tau)
# @param the usual arguments of rq()
rql <- function(formula, tau, data, weights, ...) {
  o <- plyr::llply(tau, function(tau, formula, data, weights, ...) {
    data$weights <- weights
    rq(formula=formula, tau=tau, data=data, weights=weights, ...)
  }, formula=formula, d=data, w=weights, ...)
  class(o) <- c("rql", "list")
  return(o)
}

# Predict fitted values (and possibly CIs) for lists of rq objects
#
# @param object list of rq() objects, returned by rql
# @param ... passed to ldply and predict.rq (NB: .parallel can be used)
predict.rql <- function(object, ...) {
  p <- plyr::ldply(object, function(x, ...) {
    p <- predict.rq(x, ...)
    if (is.vector(p)) {
      p <- data.frame(fit=p)
    } else {
      p <- as.data.frame(p)
    }
    p$tau <- x$tau
    return(p)
  }, ...)
  p$tau <- factor(p$tau)
  return(p)
}

# data("engel")
# m <- rql(foodexp ~ income, data=engel, tau=c(.25, .5, .75))
# system.time(p <- predict(m, newdata=engel, int="conf", se="boot", bsmethod="xy", R=1000))
# p$income <- engel$income
# ggplot() +
#   geom_point(aes(income, foodexp), data=engel) +
#   geom_ribbon(aes(income, ymin=lower, ymax=higher, fill=tau), data=p, alpha=0.5) +
#   geom_path(aes(income, fit, colour=tau), data=p)


## Non parametric quantile regression ----

# Locally linear non-parametric quantile regression
#
# @param x univariate explanatory variable
# @param y univariate response variable
# @param tau quantile(s)
# @param bw bandwidth, i.e. scale of the smooth (larger means smoother)
# @param .parallel, .progress passed to ldply()
# @param ... passed to `predict.rq`
llrq <- function(x, y, tau=.5, bw=diff(range(x))/10, n=50, .parallel=FALSE, .progress="none", ...) {
  # create the vector of output points
  xx <- seq(min(x), max(x), length.out=n)
  # for each tau
  # NB: it is faster to split by tau than to use rql inside (i.e. fit for each tau separately inside)
  #     we can't fit for all taus and then get the predictions + CI separately for each tau (fails)
  #     in addition, parallelising at this stage is the easiest and most efficient
  ldply(tau, function(tau) {
    p <- ldply(xx, function(xx) {
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
    })
    # identify x values and quantile
    p$x <- xx
    p$tau <- factor(tau)
    return(p)
  }, .parallel=.parallel, .progress=.progress)
}

# data("mcycle", package="MASS")
#
# rq_fit <- llrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3)
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_line(aes(x=x, y=`1`, colour=tau), data=rq_fit)
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
  fit <- predict(m, newdata=xx, ...)
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
