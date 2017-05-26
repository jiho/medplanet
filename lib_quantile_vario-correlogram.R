#
# Compute quantile based vario or correlograms
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

#' Quantilogram
#'
#' @param y a vector representing a regular, univariate, time series.
#' @param tau a vector of quantles to test
#' @param lag.max maximum lag at which to calculate the autocorrelation of quantiles.
#'
#' @references
#' Linton, O. and Whang, Y.-J. The quantilogram: with an application to evaluating directional predictability. Journal of Econometrics, 141(1):250--282, 2007.
#' http://www.sciencedirect.com/science/article/pii/S0304407607000152
#' https://sites.google.com/site/whangyjhomepage/research/software
#'
#' @examples
#' # Create data with mostly noise and a periodic signal for high values only
#' set.seed(12)
#' n <- 200
#' x <- 1:n
#' y1 <- runif(n, -1, 1)
#' y2 <- cos(x/(n/10)*pi) * 1.05
#' y <- ifelse(y2 > 1, y2, y1)
#' plot(x, y, "l")
#' qm <- quantilogram(y, tau=c(0.5, 0.9), lag.max=50)
#' plot(qm)
quantilogram <- function(y, tau=c(0.25, 0.5, 0.75, 0.95), lag.max=10*log10(length(y))) {

  n <- length(y)
  lag.max <- round(lag.max)
  talpha <- tau
  nalpha <- length(tau)

  signum <- mat.or.vec(lag.max+1, nalpha)
  Vk <- mat.or.vec(nalpha, 1)

  # compute sample quantiles
  muhat <- quant(y, talpha)

  for (jalpha in 1:nalpha) {
    # select the current quantile
    alpha <- talpha[jalpha]
    # anomaly to the quantile
    epshat <- y - muhat[jalpha]
    # check function
    check <- (epshat > 0) - (epshat < 0) - 1 + 2*alpha
    for (k in 0:lag.max){
      sign_sign <- check[1:(n-k)]*check[(k+1):n]
      signum[k+1, jalpha]  <-  mean(sign_sign)/mean(check[1:(n-k)]^2)
    }
    Vk[jalpha] <- 1 + ( max(c(alpha,1-alpha))^2 )/(alpha*(1-alpha))
  }

  colnames(signum) <- tau
  bp <- apply((signum^2),2,cumsum)

  sel <- sqrt(1/n)
  seu <- sqrt(Vk/n)

  out <- list(
    tau=tau,
    lag=0:lag.max,
    quantilogram=signum,
    box.ljung=bp,
    sel=sel,
    seu=seu
  )
  class(out) <- "quantilogram"

  return(out)
}

plot.quantilogram <- function(x, alpha=0.05) {
  library("ggplot2")
  X <- gather(x$quantilogram, key="tau", value="value", -lag)
  X$seu <- rep(x$seu, each=nrow(x$quantilogram))

  ggplot(X) + facet_wrap(~tau) +
    geom_hline(yintercept=x$sel * qnorm(alpha/2) * c(-1,1), colour="#3366FF") +
    geom_hline(aes(yintercept=seu * qnorm(alpha/2) * c(-1,1)), colour="#3366FF", linetype="dashed") +
    geom_linerange(aes(x=lag, ymin=0, ymax=value))
}


#' Empirical quantile computation
#'
#' Fast alternative to quantile(x, probs, type=1)
#'
#' @param x numeric vector.
#' @param probs numeric vector of probabilities with values in [0,1].
quant <- function(x, probs){
  nq <- round(probs*length(x));
  x_sorted <- sort(x) ;
  q <- x_sorted[nq] ;
  return(q) ;
}


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


# x <- 1:100
# y <- sin(x/100*4*pi)
# plot(x, y)
#
# acf(y, lag.max=70)
# acf(sample(y), lag.max=70)
#
# a <- correlogram(x, y, dist.max=70, bin=1); barplot(a$acf)
# a <- weighted_correlogram(sample(x), y, dist.max=70, bin=1); barplot(a$acf)
# a <- weighted_correlogram(x, sample(y), dist.max=70, bin=1); barplot(a$acf)
# w <- dnorm(y-quantile(y, 0.5), sd=0.3)
# a <- weighted_correlogram(x, y, w, dist.max=70, bin=1); barplot(a$acf)
# a <- weighted_correlogram(sample(x), y, w, dist.max=70, bin=1); barplot(a$acf)
#
# x <- 1:100
# y <- runif(length(y))
# plot(x, y)
# acf(y, lag.max=70)
# w <- dnorm(y-quantile(y, 0.75), sd=0.3)
# a <- weighted_correlogram(x, y, w, dist.max=70, bin=1); barplot(a$acf)
#


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
