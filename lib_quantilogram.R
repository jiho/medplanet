#
# Compute quantilogram
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
      dplyr::summarise(signum=mean(sign) / mean(check[col]^2), alpha=alpha, n=n())
      # NB: given how lower.tri works, the reference (1:(n-k) in the original implementation) are the col index here, not the row index
  })

  # convert to wide format
  signum <- tidyr::spread(signum, key="alpha", value="signum")

  # extract distance
  dx <- signum$dx
  n.used <- signum$n
  signum <- dplyr::select(signum, -dx, -n)

  # compute box.ljung and other statistics
  bp <- apply((signum^2),2,cumsum)
  sel <- sqrt(1/n)

  out <- list(
    tau=tau,
    dx=dx,
    n.used=n.used,
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
