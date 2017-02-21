#
# Utility functions for quantile regression
#
# non-parametric regression functions are inspired by vignette("rq")
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("quantreg")

## Tools ----

# Extract a single quantile from a multiple quantile object
# @param x object of class rqs
# @param tau quantile to extract
as.rq <- function(x, tau) {
  if (!"rqs" %in% class(x)) {
    stop("Need an object of class rqs")
  }
  i <- match(tau, x$tau)
  if (is.na(i)) {
    stop("Cannot find quantile ", tau, ". Available quantiles are: ", paste0(x$tau, collapse=", "), ".")
  }

  # subset everything
  x$coefficients <- x$coefficients[,i]
  x$fitted.values <- x$fitted.values[,i]
  x$residuals <- x$residuals[,i]
  x$tau <- x$tau[i]
  x$rho <- x$rho[i]
  # assign class
  class(x) <- "rq"

  return(x)
}

# Better implementation of predict for multiple quantiles regression
# = extracts the same information as predict.rq, for each quantile
# @param object object of class rqs
# @param ... passed to predict.rq
predict.rqs <- function(object, ...) {
  fit <- ldply(object$tau, function(tau) {
    # extract the fit for this quantile
    fit <- quantreg::predict.rq(as.rq(object, tau), ...)
    # when no confidence interval is extracted, the object is a single vector of fitted values
    # make it a data.frame to be compatible
    if (is.vector(fit)) {
      fit <- data.frame(fit=fit)
    }
    data.frame(fit, tau)
  })
  fit$tau <- factor(fit$tau)
  return(fit)
}

# data("engel")
# m <- rq(foodexp ~ income, data=engel, tau=c(.25, .5, .75))
# head(p <- predict(m))
# head(p <- predict(m, newdata=engel, interval="confidence"))
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
# @param ... passed to `predict.rq`
llrq <- function(x, y, tau=.5, bw=diff(range(x))/10, n=50, ...) {
  # create the vector of output points
  xx <- seq(min(x), max(x), length.out=n)
  ldply(xx, function(xx) {
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
    p$x <- xx
    return(p)
  })
}

# library("MASS")
# data("mcycle")
#
# rq_fit <- lprq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3)
# ggplot() +
#   geom_point(aes(x=times, y=accel), data=mcycle) +
#   geom_line(aes(x=x, y=fit, colour=tau), data=rq_fit)
#
# rq_fit <- lrq(mcycle$times, mcycle$accel, tau=c(.25, .5, .75), bw=3, interval="confidence", se="boot")
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
