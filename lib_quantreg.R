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

# Extract a single quantile from a multiple quantile object
#
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
#
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


## Quantile-based ANOVA ----

# aov()-like function based on quantile regression
#
# @param formula a formula object, like for aov()
# @param data a data.frame in which to interpret the variables named in the formula
# @param tau the quantile(s) to be estimated
# @param ... passed to anova.rq
aovq <- function(formula, data, tau, ...) {
  ans <- plyr::llply(tau, function(tau) {
    # compute regression and null model for this quantile
    suppressWarnings(m <- rq(formula, tau=tau, data=data))
    suppressWarnings(mnull <- rq(update(formula, . ~ 1), tau=tau, data=data))
    # compare them
    suppressWarnings(a <- anova(mnull, m, ...))

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
