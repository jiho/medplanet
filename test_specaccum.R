#
# Test various testing approaches for species accumulation curves
#
# (c) 2017 Jean-Olivier Irisson, GNU General Public License v3
#

library("plyr")
library("tidyverse")
library("vegan")

# Exploration of vegan::specaccum options ----

data(BCI)

# exact
system.time(sp1 <- specaccum(BCI))
plot(sp1)
n <- nrow(BCI)
ex <- data.frame(sites=1:n, richness=predict(sp1))

spb <- specaccum(BCI > 0)
plot(spb)
# -> identical => does not deal with abundances

# random
system.time(sp <- specaccum(BCI, method="random", permutations=1000))
df <- as.data.frame(sp[c("sites", "richness", "sd")])
stats <- adply(sp$perm, 1, function(x) {
  data.frame(
    mean(x),
    sd(x),
    median(x),
    t(quantile(x, c(0.025, 0.975), names=F))
  )
})
df <- cbind(df, stats)

all(df$richness == df$mean.x.)
all(df$sd == df$sd.x.)
# -> the stats are just computed from the permutations


ggplot() +
  # geom_step(aes(sites, rich, group=i), alpha=0.05) +
  geom_ribbon(aes(sites, ymin=X1, ymax=X2), data=df, alpha=0.2, fill="blue") +
  geom_path(aes(sites, richness), data=df, colour="blue") +
  geom_path(aes(sites, richness), data=ex, colour="red")
# -> exact and mean of random are very close

boxplot(sp)


# Compute confience interval ----

load("data.rda")
dd <- d0 %>% group_by(site, date, species) %>% summarise(cpue=sum(cpue)) %>% ungroup()
dd <- filter(dd, !is.na(species))
dd <- arrange(dd, site, date)

dw <- spread(dd, key="species", value="cpue", fill=0)
dw <- select(dw, -date)

# Species accumulation curves with CI
#
# @param see specaccum
accum_stats <- function(comm, permutations=1000, ...) {
  sp <- specaccum(comm, method="random", permutations=permutations, ...)
  stats <- adply(sp$perm, 1, function(x) {
    data.frame(
      richness=mean(x),
      sd=sd(x),
      ci.low=quantile(x, 0.025, names=FALSE),
      ci.high=quantile(x, 0.975, names=FALSE)
    )
  }, .id="sites")
  stats$sites <- as.numeric(stats$sites)

  return(stats)
}
stats <- dw %>% group_by(site) %>% do(accum_stats(.[,-1]))

# and some plots
ggplot(stats) + geom_ribbon(aes(x=sites, ymin=ci.low, ymax=ci.high, fill=site), alpha=0.2) + geom_path(aes(x=sites, y=richness, colour=site)) + labs(x="nights")
ggplot(stats) + geom_ribbon(aes(x=sites, ymin=richness-sd, ymax=richness+sd, fill=site), alpha=0.2) + geom_path(aes(x=sites, y=richness, colour=site)) + labs(x="nights")

## Test difference in richness among locations at a given effect ----


# effort level at which to test
n_test <- 50

# select sites valid with this number of nights
# compute permutations
sp <- specaccum(comm, method="random", permutations=1000)
perm <- sp$perm
n_nigh_per_site <- sapply(perm, nrow)
focus_sites <- names(n_nigh_per_site[n_nigh_per_site >= n_test])
dws <- filter(dw, site %in% focus_sites)

# compute richness at a given effort (number of nights)
richness_at_n <- function(d, n) {
  ddply(d, ~site, function(x) {
    sp <- specaccum(x[,-1], method="exact") # we could use random here but it is longer to compute
    sp$richness[n]
  })
}

# for the real data
ref_richness <- richness_at_n(dws, n_test)

# for 100 permutations of sites
x <- dws
reps <- laply(1:100, function(i) {
  x$site <- sample(x$site)
  r <- richness_at_n(x, n_test)
  return(t(r$V1))
}, .parallel=T)
reps <- t(reps)
# lines = sites

# select two sites
ii <- c(1,6)

ref_diff <- abs(diff(rich_ref$V1[ii]))
diffs <- apply(rep[ii,], 2, function(x) {abs(diff(x))})

p.value <- sum(diffs > diff_ref) / length(diffs)
p.value

ggplot(sp) + geom_path(aes(x=sites, y=richness, colour=site))

