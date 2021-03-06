#
# Correlogram, tentative for a weighted correlogram, variogram for irregular series
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#


#' Autocorrelation function estimation
#'
#' @details This function is just meant to duplicate the results of acf() in the case of a regular time series, to check the validity of the computation. It also extends it to non-regular time series by using a coordinate and computing the distances between all pairs of points.
#' @examples
#' y <- runif(50)
#' a1 <- acf(y, lag.max=10, plot=F)
#' a2 <- correlogram(1:length(y), y, dist.max=10, bin=1)
#' all.equal(as.numeric(a1$acf), a2$acf)
correlogram <- function(x, y, dist.max=diff(range(x, na.rm=T))/3, bin=dist.max/30) {
  # checks
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  # distance
  dx <- as.matrix(dist(x))
  dx <- dx[lower.tri(dx, diag=TRUE)]

  # correlation
  yc <- y - mean(y, na.rm=T)
  corr <- matrix(yc, ncol=1) %*% matrix(yc, nrow=1)

  # scale in -1,+1
  # scale_factor <- var(y, na.rm=TRUE) * length(y)
  scale_factor <- sum(diag(corr)) # this is equivalent
  corr <- corr / scale_factor

  # extract the numbers
  corr <- as.numeric(corr[lower.tri(corr, diag=TRUE)])
  acf <- na.omit(data.frame(dx, corr))

  # limit to a given lag
  acf <- filter(acf, dx <= dist.max)

  # compute average correlation per bin
  acf$dx <- round_any(acf$dx, bin, ceiling)
  acf <- acf %>%
    group_by(dx) %>%
    summarise(acf=sum(corr), n=length(corr))

  return(acf)
}

#' Weighted autocorrelation function estimation
#'
#' @param x vector of coordinate.
#' @param y vector of variable.
#' @param w vector of weights; by default, all equal to 1, which is equivalent to no weight.
#' @param dist.max maximum distance between points to consider to compute correlation (in unit of x). NB: everything is actually computed and only points within [0,dist.max] are returned.
#' @param bin size of the distance bin (in unit of x) to compute correlation. For a regular series, x should be the sequence of integers from 1 to length(y) and bin should be 1.
#'
#' @examples
#' # Create data with mostly noise in the median values and a periodic
#' # signal in the extreme (high and low) values
#' set.seed(12)
#' n <- 200
#' x <- 1:n
#' y1 <- runif(n, -1, 1)
#' y2 <- cos(x/(n/10)*pi) * 1.05
#' y <- ifelse(y2 > 1 | y2 < -1, y2, y1)
#' plot(x, y, "l")
#' lines(predict(loess(y~x, span=0.2)), col="blue")
#'
#' # compute regular autocorrelation
#' a <- acf(y, lag.max=40, plot=FALSE)
#' barplot(as.numeric(a$acf))
#' a <- weighted_correlogram(x, y, dist.max=40, bin=1)
#' barplot(a$acf)
#' # cyclic signal of period 40
#'
#' # focus on median values
#' w <- dnorm(y - quantile(y, 0.5), sd=0.2)
#' a <- weighted_correlogram(x, y, w=w, dist.max=40, bin=1)
#' barplot(a$acf)
#' # no more autocorrelation = just the noise
#'
#' # add weight around extreme values
#' w <- dnorm(y - quantile(y, 0.9), sd=0.2)
#' a <- weighted_correlogram(x, y, w=w, dist.max=40, bin=1)
#' barplot(a$acf)
#' w <- dnorm(y - quantile(y, 0.1), sd=0.2)
#' a <- weighted_correlogram(x, y, w=w, dist.max=40, bin=1)
#' barplot(a$acf)
#' # stronger periodic signal at both extremes
weighted_correlogram <- function(x, y, w=rep(1, length(y)), dist.max=diff(range(x, na.rm=T))/3, bin=dist.max/30) {
  # checks
  if (length(x) != length(y) | length(y) != length(w)) {
    stop("x, y, and w must have the same length")
  }

  # distance
  dx <- as.matrix(dist(x))
  dx <- dx[lower.tri(dx, diag=TRUE)]

  # correlation
  yc <- y - weighted.mean(y, w, na.rm=T)
  # TODO shouldn't it be the quantile here? or the weighted mean?
  corr <- matrix(yc, ncol=1) %*% matrix(yc, nrow=1)

  # weights
  # w <- w / sum(w, na.rm=TRUE)
  # w <- w / max(w, na.rm=TRUE)
  # NB: given how we scale the correlation afterwards, any weighting vector is appropriate
  w <- matrix(w, ncol=1) %*% matrix(w, nrow=1)

  # apply weights
  corr <- corr * w

  # scale in -1,+1
  corr <- corr / sum(diag(corr), na.rm=T)

  # extract the numbers
  corr <- as.numeric(corr[lower.tri(corr, diag=TRUE)])
  acf <- data.frame(dx, corr)
  # acf <- na.omit(acf)

  # limit to a given lag
  acf <- filter(acf, dx <= dist.max)

  # compute average correlation per bin
  acf$dx <- round_any(acf$dx, bin, ceiling)
  acf <- acf %>%
    group_by(dx) %>%
    summarise(acf=sum(corr), n=length(corr))

  return(acf)
}


# variogram <- function(x, y, dist.max=diff(range(x, na.rm=T))/3, cloud=FALSE, bin=dist.max/30) {
#   # distance
#   dx <- as.numeric(dist(x))
#   # semi-variance
#   # var <- outer(y, y, function(X, Y) ((X - Y)^2)/2)
#   # var <- as.numeric(var[lower.tri(var)])
#   # or
#   var <- as.numeric(dist(y))^2 / 2
#   # variogram cloud
#   vario <- data.frame(dx, var)
#
#
#   # limit to a given distance
#   vario <- dplyr::filter(vario, dx <= dist.max)
#
#   if (!cloud) {
#     # round lags
#     vario$dx <- round_any(vario$dx, bin, round)
#     # sample variogram
#     vario <- vario %>% group_by(dx) %>% summarise(semivariance=mean(var, na.rm=T), n=sum(!is.na(var)))
#   }
#   return(vario)
# }
#
# v <- pastecs::vario(lh)
# v <- variogram(1:length(lh), lh, bin=1, dist.max=16)
# plot(semivariance ~ dx, data=v, type="l")
