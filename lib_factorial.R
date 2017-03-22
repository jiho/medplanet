#
# Function to ease factorial analyses and plots
#

# turn a data.frame like tidy object into a multivariate version
as.multivar <- function(x, x.var, y.var="species", value="mean") {
  xd <- reshape2::dcast(data=x, as.formula(str_c(x.var, "~", y.var)), value.var=value, fill=0)
  rownames(xd) <- xd[,x.var]
  xd <- dplyr::select_(xd, str_c("-", x.var))
  return(xd)
}


# extract and plot result from FactoMineR::CA
library("broom")  # for the generics for tidy, augment, etc.

tidy.CA <- function(x) {
  d <- x$eig
  names(d) <- c("eigenvalue", "prop.variance", "cum.prop.variance")
  d[,2:3] <- d[,2:3]/100
  d$term <- stringr::str_c("Dim", 1:nrow(d))
  row.names(d) <- NULL
  return(d)
}

augment.CA <- function(x, dimensions=c(1,2,3,4)) {
  d <- plyr::ldply(c("row", "col"), function(type) {
    d <- data.frame(x[[type]]$coord)[,dimensions]
    names(d) <- str_c("Dim", dimensions)
    d$type <- type
    d$label <- row.names(d)
    d$cos2 <- rowSums(x[[type]]$cos2[,dimensions])
    return(d)
  })
}

autoplot.CA <- function(x, dimensions=c(1,2), size=1, ...) {
  library("ggplot2")
  dat <- augment(x, dimensions=dimensions)
  eig <- tidy(x)
  dims <- names(dat)[str_detect(names(dat), "Dim")]
  labs <- str_c(dims, " (",round(eig$prop.variance[dimensions]*100),"%)")
  p <- ggplot(dat, aes_string(x=dims[1], y=dims[2], colour="type")) + scale_colour_discrete(guide="none") + geom_point(size=size, ...) + ggrepel::geom_text_repel(aes(label=label), size=size*2.5, segment.alpha=0.7, ...) + scale_x_continuous(breaks=0) + scale_y_continuous(breaks=0) + coord_fixed() + labs(x=labs[1], y=labs[2])
  return(p)
}
