library("plyr")
library("dplyr")
# library("lubridate")

load("../../data.Rdata")
# load("data.Rdata")

all_families <- sort(unique(taxo$family))
all_genera   <- sort(unique(taxo$genus))
all_species  <- sort(unique(taxo$species))

all_places <- unique(select(d, place, lat, lon))

# # summarise data per week
# d <- summarise(group_by(d, weeks_since_start, place, lat, lon, family, genus, species), cpue=mean(cpue, na.rm=T))
# d <- ungroup(d)
#
# # ggplot(filter(d, species=="Chromis chromis"), aes(x=weeks_since_start, y=cpue, colour=place)) + geom_line() + geom_point() + facet_wrap(~place) + scale_y_continuous(trans="log1p")
#
#
# # add NAs for unsampled weeks
# d1 <- unique(select(d, place, lat, lon, family, genus, species))
# d1$i <- 0
# weeks_since_start <- seq(from=min(d$weeks_since_start), to=max(d$weeks_since_start), by=1)
# d2 <- data.frame(weeks_since_start, i=0)
# dd <- left_join(d2, d1)
# dd <- select(dd, -i)
# dd <- left_join(dd, d)
#
# # ggplot(filter(dd, species=="Chromis chromis"), aes(x=weeks_since_start, y=cpue, colour=place)) + geom_line(size=0.25, alpha=0.7) + geom_point(size=1) + facet_wrap(~place) + scale_y_continuous(trans="log1p")
#
# d <- dd
#
# save(d, file="weekly_data.Rdata")
load("../../weekly_data.Rdata")