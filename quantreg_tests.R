#
# Quantile regression tests
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

source("lib_quantreg.R")
library("logspline")
library("stringr")
library("plyr")
library("tidyverse")
library("broom")

# Multiply numbers by an increasing factor to expand values outwards
# NB: with f<1, this contracts numbers
mult <- function(x, f) {
  fs <- seq(from=1, to=f, length.out=length(x))
  x <- x * fs[rank(x)]
  return(x)
}
# mult(1:5, 1.5)
# (x <- sample(1:5))
# mult(x, 1.5)


## Continuous quantile regression ----

# make data up
set.seed(12)
n <- 150
d <- data.frame(x=runif(n), y=runif(n))
qplot(x, y, data=d)
# expand y upwards for increasing values of x, but only above the median
# NB: this is done withing bins of x
n_bin <- 8
f <- seq(1, 1.7, length=n_bin)
d$bin <- cut(d$x, n_bin, labels=F)
for (i in 1:n_bin) {
  X <- d[d$bin == i,]
  X$y[X$y > quantile(X$y, 0.5)] <- mult(X$y[X$y > quantile(X$y, 0.5)], f[i])
  d[d$bin == i,] <- X
}
qplot(x, y, data=d)

# performs the regression
m <- rq(y ~ x, data=d, tau=c(0.25, 0.5, 0.9))

# tests the effect
# NB: tidy() calls summary() but returns a nicely formated data.frame
filter(tidy(m, se="rank", iid=T), term=="x")
filter(tidy(m, se="rank", iid=F), term=="x")
filter(tidy(m, se="iid"), term=="x")
filter(tidy(m, se="nid"), term=="x")
filter(tidy(m, se="ker"), term=="x")
filter(tidy(m, se="boot", R=1000, bsmethod="xy"), term=="x")
filter(tidy(m, se="boot", R=1000, bsmethod="pwy"), term=="x")
filter(tidy(m, se="boot", R=1000, bsmethod="mcmb"), term=="x")
filter(tidy(m, se="boot", R=1000, bsmethod="wxy"), term=="x")
# -> all give the same qualitative result (0.9 signif, the others not; which is what we want)
#    nid or boot with bsmethod xy or wxy is apparently more powerfull

# plot predictions
xx <- data.frame(x=seq(0, 1, length=100))
p <- predict(m, newdata=xx, interval="confidence", se="nid")
# NB: this is a custom version of predict.rqs
p$x <- xx$x

ggplot() +
  geom_point(aes(x=x, y=y), d) +
  geom_ribbon(aes(x=x, ymin=lower, ymax=higher, fill=tau), p, alpha=0.4) +
  geom_path(aes(x=x, y=fit, colour=tau), p)


## Discrete variable with two levels ----

# make data up
set.seed(1)
x1 <- x2 <- rnorm(100, mean=5)
x1[x1 > quantile(x1, 0.5)] <- mult(x1[x1 > quantile(x1, 0.5)], 1)
x2[x2 > quantile(x2, 0.5)] <- mult(x2[x2 > quantile(x2, 0.5)], 1.2)

d <- data.frame(x1, x2)
d <- gather(d, key="x", value="y")
d$x <- factor(d$x)
qplot(x, y, data=d) + geom_quantiled(quantiles=c(0.25, 0.5, 0.75, 0.9), size=4)
qplot(x, y, data=d) + geom_quantiled(aes(colour=..quantile..), quantiles=c(0.25, 0.5, 0.75, 0.9), size=4)

d %>% group_by(x) %>% summarise(mean(y), median(y))

wilcox.test(y ~ x, d)
t.test(y ~ x, d)
ks.test(x1, x2)

m <- rq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d)
filter(tidy(m, se="rank", iid=T), term=="xx2")
filter(tidy(m, se="rank", iid=F), term=="xx2")
filter(tidy(m, se="iid"), term=="xx2")
filter(tidy(m, se="nid"), term=="xx2")
filter(tidy(m, se="ker"), term=="xx2")
filter(tidy(m, se="boot", R=1000, bsmethod="xy"), term=="xx2")
filter(tidy(m, se="boot", R=1000, bsmethod="pwy"), term=="xx2")
filter(tidy(m, se="boot", R=1000, bsmethod="mcmb"), term=="xx2")
filter(tidy(m, se="boot", R=1000, bsmethod="wxy"), term=="xx2")
# -> Effet non-significant for the median and for the full distribution
#    Barely significant for the mean
#    Not significant for q0.5 but significant for q0.75 highly significant for q09 (which is what we expect)
#    All methods give similar results but iid and ker seem more powerful. But iid at least is likely not appropriate
#    All bootstraps are quite consistent with each other

aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="Wald")
aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="anowar", R=1000)
aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="rank")
# -> Wald here is equivalent to nid above (which is expected based on the specificatons in the book)
#    anowar seems more "progressive" (i.e. more detailed than rank)


## Discrete variable with more than two levels ----
# ANOVA- or Kruskall- like

set.seed(1)
x1 <- x2 <- x3 <- rnorm(100, mean=5)
x1[x1 > quantile(x1, 0.5)] <- mult(x1[x1 > quantile(x1, 0.5)], 1)
x2[x2 > quantile(x2, 0.5)] <- mult(x2[x2 > quantile(x2, 0.5)], 0.9)
x3[x3 > quantile(x3, 0.5)] <- mult(x3[x3 > quantile(x3, 0.5)], 1.1)

d <- data.frame(x1, x2, x3)
d <- gather(d, key="x", value="y")
d$x <- factor(d$x)
qplot(x, y, data=d)

kruskal.test(y ~ x, data=d)
summary(aov(y ~ x, data=d))

aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="Wald")
aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="anowar", R=1000)
aovq(y ~ x, tau=c(0.5, 0.75, 0.9), data=d, test="rank")
# -> Kruskall and ANOVA both not significant
#    Quantile equivalent is, for q0.75 and q0.9 only, as expected

## Conclusions ----
#
# Use rq + summary.rq(se="boot", R=1000, bsmethod="xy") to test regressions
# Use aovq(test="anowar", R=1000) to test for effects of discrete variables
#
