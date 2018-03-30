#
# Read data form all data sources, add metadata and compile everything
#

library("readxl")
library("stringr")
library("plyr")
library("lubridate")
library("tidyverse")

# transform lat and lon from ºN notation to decimatl degrees
parse_latlon <- function(x) {
  library("stringr")
  # get different pieces
  bits <- str_split_fixed(x, "[º'\"]", 4)
  # remove N or E (for now)
  bits <- bits[,-4]
  # convert to numbers
  class(bits) <- "numeric"
  # convert to decimal degrees
  bits[,1] + (bits[,2] + bits[,3]/60)/60
}


## Read ECOCEAN ----

d <- read_excel("data/ecocean/Compil Donnees peches - Ecocean.xlsx", sheet=1, skip=1)
d1 <- read_excel("data/ecocean/Donnees peches Girel 2013.xlsx", sheet=1, skip=1)
d <- rbind(d, d1)

# cleanup data
d$lat <- parse_latlon(d$lat)
d$lon <- parse_latlon(d$lon)

unknown_gear_ok <- which(is.na(d$n_gear_ok))
d$n_gear_ok[unknown_gear_ok] <- d$n_gear_set[unknown_gear_ok]

# detect and remove non numerical counts
non_num <- is.na(as.numeric(d$n))
select(d, family, n)[non_num,]
# -> ok only non fish
d$n <- as.numeric(d$n)

# select relevant data
d <- select(d, date_out, site, station, project, lat, lon, n_gear_ok, family, genus, species, n)

ecocean <- d


## Read SUBLIMO ----

# read and merge all data
catches <- read_excel("data/sublimo/bdd nettoye 012015.xlsx", sheet=1, skip=1)
# env <- read_excel("data/sublimo/Environnement 2012_2015.xlsx", sheet=1, skip=1)
coord <- read_excel("data/sublimo/coordinates.xlsx", sheet=1)

d <- left_join(select(catches, site, station, year_out, month_out, day_out, family, genus, species, n), coord)

# cleanup data
d$date_out <- ymd_hms(str_c(d$year_out, "-", sprintf("%02i", d$month_out), "-", sprintf("%02i", d$day_out), "00:00:00"))
d <- select(d, -year_out, -month_out, -day_out)

d$project <- "SUBLIMO"

# specify empty catches
d$n[is.na(d$n)] <- 0
d$n_gear_ok <- 1

sublimo <- d


## Read Villefranche ----

# read and merge data
coords  <- read_excel("data/villefranche/coordinates.xlsx", sheet=1)
effort  <- read_excel("data/villefranche/effort.xlsx", sheet=1)
catches <- read_excel("data/villefranche/catches.xlsx", sheet=1)

d <- full_join(select(catches, date_out, station, gear, family, genus, species_suffix, n), select(effort, date_out, station, gear, n_gear_ok))
d <- full_join(d, coords)

# cleanup data
d <- filter(d, gear=="CARE")
d <- select(d, -gear)

d$project <- "RADEICHTHYO"
d$site <- "Villefranche"

vlfr <- d

head(vlfr)
vlfr <- dplyr::rename(vlfr, species=species_suffix)


## Merge all data, compute effort and CPUE ----

d <- bind_rows(ecocean, sublimo, vlfr)
d <- dplyr::rename(d, date=date_out, n_gear=n_gear_ok)

summary(d)

# inspect
filter(d, is.na(n))
filter(d, n>1000)
filter(d, is.na(n_gear))

# remove probably wrong identifications
#  P. pilicornis spawns during summer (not in feb)
idx <- which(d$site == "Bastia" & year(d$date) == 2013 & month(d$date) == 2 & d$species=="pilicornis")
d[idx,c("family", "genus", "species", "n")] <- rep(c(NA, NA, NA, 0), each=length(idx))
d[idx,]
# D. vulgaris spawn during fall (not in june)
idx <- which(d$species == "vulgaris" & month(d$date) == 7 & d$site == "Port Vendres")
d[idx,c("family", "genus", "species", "n")] <- rep(c(NA, NA, NA, 0), each=length(idx))
d[idx,]


# compute Catch Per Unit Effort (CPUE), per site

# compute total effort (nb of CAREs) per site
# NB: start by selecting all unique records of sampled nights (some are repeated because several larvae were caught)
effort <- unique(select(d, date, site, station, n_gear)) %>% group_by(date, site) %>% summarise(n_gear=sum(n_gear)) %>% ungroup()
# compute total catches per site
catches <- d %>% group_by(date, site, family, genus, species) %>% summarise(n=sum(n, na.rm=T)) %>% ungroup()
# compute center of gravity of CARE positions in sites
sites <- d %>% group_by(site) %>% summarise(lat=mean(lat, na.rm=T), lon=mean(lon, na.rm=T)) %>% ungroup()

d <- full_join(catches, effort)
d$cpue <- d$n / d$n_gear
# d <- select(d, -n, -n_gear)

# remove NA/NaN cpue because this should not happen (and is not informative)
d <- filter(d, !is.na(cpue))

# sort sites by lon then lat
sites <- arrange(sites, lon, lat)
d <- left_join(d, sites)
d$site <- factor(d$site, levels=unique(sites$site))
effort$site <- factor(effort$site, levels=unique(sites$site))
sites$site <- factor(sites$site, levels=unique(sites$site))

# keep only data before start of 2016
date_max <- ymd_hms("2016-01-01 00:00:00")
d <- filter(d, date < date_max)
effort <- filter(effort, date < date_max)


## Clean taxonomic names ----

# clean names written in files
d$family <- str_trim(tolower(d$family))
d$genus <- str_trim(tolower(d$genus))
d$species <- str_trim(tolower(d$species))

# remove uncertainty
d[which(str_detect(d$family, "\\?")),c("family", "genus", "species")] <- NA
d[which(str_detect(d$genus, "\\?")),c("genus", "species")] <- NA
d[which(str_detect(d$species, "\\?")),c("species")] <- NA

# remove "sp"
d$species[which(str_detect(d$species, "^sp[1-9p]*$"))] <- NA

# check that when higher level is NA, the lower levels are NA
all(is.na(filter(d, is.na(family))$genus))
all(is.na(filter(d, family=="ni")$genus))
all(is.na(filter(d, is.na(genus))$species))

# check that each genus has the same family
filter(summarise(group_by(d, genus), n=length(unique(family))), n>1)
# and most species suffix should belong to only one genus
filter(summarise(group_by(d, species), n=length(unique(genus))), n>1)
# 4 exceptions, OK.

# remove non-fish
d <- d[-which(str_detect(d$family, "^crabe")),]
d <- d[-which(str_detect(d$family, "^crevette")),]
d <- d[-which(str_detect(d$family, "^vers")),]
d <- d[-which(d$family == "squille"),]
d <- d[-which(d$family == "méduse"),]
d <- d[-which(d$family == "octopodidae"),]
d <- d[-which(d$family == "loliginidae"),]
d <- d[-which(d$family == "sepiida"),]
d <- d[-which(d$family == "sepiolidae"),]
d <- d[-which(d$family == "nephropidae"),]
d <- d[-which(d$family == "pinnotheridae"),]
d <- d[-which(d$family == "mysis"),]
d <- d[-which(d$family == "copépodes"),]

# remove non-coastal fish
d <- d[-which(d$family == "engraulidae"),]
d <- d[-which(d$family == "clupeidae"),]
d <- d[-which(d$family == "scombridae"),]
d <- d[-which(d$family == "sphyraenidae"),]
d <- d[-which(d$family == "torpedinidae"),] ## raies
d <- d[-which(d$family == "myctophidae"),] ## poissons lanternes = pélagiques
d <- d[-which(d$family == "carangidae"),]
d <- d[-which(d$family == "belonidae"),]


# # check
# print(arrange(unique(select(d, family, genus, species)), family, genus, species), n=200)
# summary(d)
# unique(d$site)
# unique(d$project)
# sort(unique(d$family))
# sort(unique(d$genus))
# sort(unique(d$species))

# cleanup taxonomic names
d$family[which(d$family == "ni")] <- NA
d$family[which(d$family == "anguilliformes")] <- NA
d$family <- str_to_title(d$family)
d$genus <- str_to_title(d$genus)

d$genus[which(d$genus=="Pagellus"&d$species=="mormyrus")] <- "Lithognathus"
d$genus[which(d$genus=="Mugilidae"&d$family=="Mugilidae")] <- NA

# abbreviated species
d$sp <- str_c(str_sub(d$genus, 1, 1), ". ", d$species)
# full species
d$species <- str_c(d$genus, " ", d$species)


## Prepare ancillary data ----

# Prepare map
range(d$lat)
range(d$lon)

# read coastline extracted on this domain
coast <- read.csv("data/gshhg_nw_med_h.csv")

# and plot it
map <- ggplot(mapping=aes(x=lon, y=lat)) + list(
  geom_polygon(data=coast, fill="white", colour="grey50", size=0.25),
  coord_quickmap(),
  scale_x_continuous(expand=c(0,0)), scale_y_continuous(expand=c(0,0)),
  theme(
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank()
  )
)
map


# manually define regions based on topography, geography, or ocean features
sites$topography <- "Gulf of Lion"
sites$topography[which(sites$lon >= 5.7)] <- "Ligurian sea"   # east of Cap sicié
sites$topography <- factor(sites$topography)

sites$rhone <- "West of Rhone"
sites$rhone[which(sites$lon >= 4.8)] <- "East of Rhone"
sites$rhone <- factor(sites$rhone, levels=sort(unique(sites$rhone), decreasing = TRUE))

sites$region <- c(
  rep("Gulf of Lion", 3),
  rep("Marseille et al", 4),
  rep("Var", 2),
  rep("Alpes Mar.", 1),
  rep("Corse", 3)
)
sites$region <- factor(sites$region, levels=unique(sites$region))


# set position to avoid overplotting
sites$hjust <- ifelse(sites$lon<4, -0.1, 1.1)
sites$hjust[sites$site %in% c("Marseille", "La Ciotat")] <- -0.1
sites$vjust <- 0.5
sites$vjust[sites$site %in% c("Les Embiez", "Port-Cros")] <- 1
sites$vjust[sites$site %in% c("Bastia", "Carry")] <- 0

# plot sites to check
map + geom_point(aes(colour=topography), data=sites) + geom_text(aes(colour=topography, label=site, hjust=hjust, vjust=vjust), data=sites, size=3)
map + geom_point(aes(colour=region), data=sites) + geom_text(aes(colour=region, label=site, hjust=hjust, vjust=vjust), data=sites, size=3)

# extract date components
d$year <- factor(year(d$date))
d$month <- factor(month(d$date))
d$yday <- yday(d$date)
d$yweek <- week(d$date)
start <- as.Date(str_c(min(year(d$date)), "-01-01"))
d$days_since_start <- as.numeric(difftime(d$date, start, units="days"))
d$weeks_since_start <- as.numeric(ceiling(difftime(d$date, start, units="weeks")))


## Add zero catches for all non-observed taxa ----

# check that all n==0 correspond to no identification (used to record effort)
filter(filter(d, n==0), !is.na(family))

# compile complete taxonomy
taxo <- unique(select(d, family, genus, species, sp))
taxo <- arrange(taxo, family, genus, species, sp)

d0 <- ddply(d, ~date+n_gear+year+month+yday+yweek+days_since_start+weeks_since_start+site+lon+lat, function(x) {
  x <- full_join(select(x, family, genus, species, cpue), taxo, by=c("family", "genus", "species"))
  x$cpue[is.na(x$cpue)] <- 0
  return(x)
}, .progress="text")

# remove lines with only NA taxonomic specification (were used to specify 0 catches)
d0 <- d0[-which(is.na(d0$family) & is.na(d0$genus) &  is.na(d0$species)),]
d <- d[-which(is.na(d$family) & is.na(d$genus) &  is.na(d$species)),]

# remove 0 catches from d
d <- filter(d, cpue != 0)


## Save data ----

save(d, d0, effort, sites, basemap, map, taxo, file="data.rda")
