#
# Compute quantile based vario or correlograms
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

#' Quantilogram
#'
#' @param x vector, the coordinates of the data series.
#' @param y vector, the values of the data series.
#' @param tau vector of quantiles around which to compute the autocorrelation.
#' @param dist.max maximum distance (in units of x) at which to calculate the autocorrelation of quantiles.
#' @param bins number of bins over which the autocorrelation should be computed, the size of the bins is computed as \code{dist.max / bins}.
#' @param binwidth size of bins over which the autocorrelation should be computed; overrides \code{bins}.
#' @param breaks upper bounds of the bins over which the autocorrelation should be computed; overrides \code{bins} and \code{binwidth}.
#'
#' @details The implementation is a generalisation of Linton and Whang 2007 for irregularly spaced data. The autocorrelation around each quantile is computed over bins of distance along \code{x}. Autocorrelation at distance 0 is 1 by definition. Beyond that, each bin in open on the left side. For example, with \code{breaks = c(0,1,3)}, autocorrelation at dist=0 equals 1; autocorrelation at dist=1 is the standardised mean of the autocorrelation between all couples of points such that the distance between them is in ]0,1]; autocorrelation at dist=3 is computed from couples of points with distances in ]1,3].
#'
#' @references
#' Linton, O. and Whang, Y.-J. The quantilogram: with an application to evaluating directional predictability. Journal of Econometrics, 141(1):250--282, 2007.
#' http://www.sciencedirect.com/science/article/pii/S0304407607000152
#' https://sites.google.com/site/whangyjhomepage/research/software
#'
#' @examples
#' # Create data with mostly noise and signal of period 40 for high values only
#' set.seed(1)
#' n <- 200
#' x <- 1:n
#' y1 <- runif(n, -1, 1)
#' y2 <- cos(x/40*2*pi) * 1.05
#' y <- ifelse(y2 > 1, y2, y1)
#' plot(x, y, "l")
#' q <- c(0.25, 0.5, 0.75, 0.9)
#'
#' # Compute and plot basic quantligogram
#' qm <- quantilogram(x, y, tau=q, dist.max=50, binwidth=1)
#' plot(qm)
#' # change bins
#' qm <- quantilogram(x, y, tau=q, dist.max=50, bins=30)
#' plot(qm)
#' qm <- quantilogram(x, y, tau=q, dist.max=50, breaks=c(1:10, seq(10, 50, by=5)))
#' plot(qm)
#' # NB: with such irregular intervals, the confidence interval around 0 is overestimated
#'
#' \dontrun{
#' # Bootstrap test
#' bqm <- plyr::laply(1:1000, function(i) {
#'   as.matrix(quantilogram(x, sample(y), tau=q, dist.max=50, breaks=c(1:10, seq(10, 50, by=5)))$quantilogram)
#' }, .progress="text")
#' dqm <- reshape2::melt(qm$quantilogram, variable.name="tau")
#' dqm$dist <- qm$dx
#' dqm$q1 <- reshape2::melt(plyr::aaply(bqm, 2:3, quantile, probs=0.025))$value
#' dqm$q2 <- reshape2::melt(plyr::aaply(bqm, 2:3, quantile, probs=0.975))$value
#' ggplot(dqm) + facet_wrap(~tau) +
#'   geom_ribbon(aes(x=dist, ymin=q1, ymax=q2), alpha=0.3) +
#'   geom_linerange(aes(x=dist, ymin=0, ymax=value))
#' }
quantilogram <- function(x, y, tau=c(0.25, 0.5, 0.75, 0.95), dist.max=1/3*diff(range(x, na.rm=TRUE)), bins=30, binwidth=NULL, breaks=NULL, ...) {

  library("plyr")
  library("dplyr")
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  n <- length(y)

  # matrix of distance among points
  dx <- as.matrix(dist(x))
  # indexes
  ij <- which(lower.tri(dx, diag=TRUE), arr.ind=T)
  # content
  dx <- dx[lower.tri(dx, diag=TRUE)]

  # compute bins of distance
  if (!is.null(breaks)) {
    breaks <- sort(unique(c(0, breaks)))
    dx <- cut(dx, breaks=c(-1, breaks), labels=FALSE)
    dx <- breaks[dx]
  } else {
    if (is.null(binwidth)) {
      binwidth <- dist.max / bins
    }
    dx <- plyr::round_any(dx, binwidth, ceiling)
  }

  # compute sample quantiles
  muhat <- quant(y, tau)

  signum <- plyr::ldply(seq_along(tau), function(i) {
    alpha <- tau[i]
    # anomaly to the quantile
    epshat <- y - muhat[i]

    # check function
    check <- (epshat > 0) - (epshat < 0) - 1 + 2*alpha
    sign <- check %*% t(check)
    sign <- sign[lower.tri(sign, diag=TRUE)]

    qm <- data.frame(ij, dx, sign)

    # limit to a the maximum distance
    qm <- dplyr::filter(qm, dx <= dist.max)

    # compute average quantilogram per bin
    qm <- qm %>%
      dplyr::group_by(dx) %>%
      dplyr::summarise(signum=mean(sign) / mean(check[col]^2), alpha=alpha)
      # NB: given how lower.tri works, the reference (1:(n-k) in the original implementation) are the col index here, not the row index
  })

  # convert to wide format
  signum <- tidyr::spread(signum, key="alpha", value="signum")

  # extract distance
  dx <- signum$dx
  signum <- signum[,-1]

  # compute box.ljung and other statistics
  bp <- apply((signum^2),2,cumsum)
  sel <- sqrt(1/n)

  out <- list(
    tau=tau,
    dx=dx,
    quantilogram=signum,
    box.ljung=bp,
    sel=sel
  )
  class(out) <- "quantilogram"

  return(out)
}

plot.quantilogram <- function(x, alpha=0.05) {
  library("ggplot2")
  # reformat data
  x$quantilogram$dist <- x$dx
  X <- tidyr::gather(x$quantilogram, key="tau", value="value", -dist)
  # and plot it
  ggplot(X) + facet_wrap(~tau) +
    geom_hline(yintercept=x$sel * qnorm(alpha/2) * c(-1,1), colour="#3366FF") +
    geom_linerange(aes(x=dist, ymin=0, ymax=value))
}


quanto <- function(y, talpha) {
  y <- data.matrix(y)
  talpha <- as.matrix(talpha)

  n <- nrow(y) ;
  nk <- 100 ;
  ll  <-  seq(from=1,by=1,length.out=nk)/nk ;

  nalpha  <-  nrow(talpha) ;
  sign_sign <- mat.or.vec(n,1) ;
  signum <- mat.or.vec(nk,nalpha) ;
  psignum <- mat.or.vec(nk,nalpha) ;
  Vk <- mat.or.vec(nalpha,1) ;
  seu <- mat.or.vec(nalpha,1) ;
  bp <- mat.or.vec(nk,nalpha) ;

  jalpha  <-  1;


  while (jalpha<=nalpha) {
    alpha  <-  talpha[jalpha]

    #	muhat <- quantile(y,alpha,type=2)
    muhat <- quant(y,alpha)
    epshat <-  y - muhat*as.matrix(rep(1,n))


    check <- (epshat >0) - (epshat <0) -(1-2*alpha)*as.matrix(rep(1,n))
    sign_sign <- as.matrix(mat.or.vec(n-1,1))
    sign_sign <- check[1:(n-1)]*check[2:n]
    signum[1,jalpha]  <-  mean(sign_sign)/mean(check[1:(n-1)]^2)
    a <- as.matrix(mat.or.vec(nk,1)) ;
    k <- 2 ;
    while (k<=nk){
      sign_sign <- as.matrix(mat.or.vec(n-k,1))
      sign_sign <- check[1:(n-k)]*check[(k+1):n]
      signum[k,jalpha]  <-  mean(sign_sign)/mean(check[1:(n-k)]^2)
      a <- solve(toeplitz( c(1,signum[1:(k-1),jalpha])  ))%*%signum[1:k,jalpha]
      psignum[k,jalpha] <- a[k] ;

      k <- k+1 ;
    }

    #/* ssignum=w*signum[.,alpha] ; */


    Vk[jalpha] <- 1 +  ( max(  c(alpha,1-alpha))^2 )/(alpha*(1-alpha))

    seu[jalpha] <- sqrt(Vk[jalpha]/n)

    jalpha <- jalpha+1


  }

  msign <- signum[,5]


  bp <- apply((signum^2),2,cumsum)
  bpp <- apply((psignum^2),2,cumsum)

  sel <- sqrt(1/n)


  x <- ll*nk ;

  return(list(quantilogram=signum, seu=seu, sel=sel))
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
