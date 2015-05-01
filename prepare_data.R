library("readr")
library("readxl")
library("plyr")
library("dplyr")
library("lubridate")
library("reshape2")
library("tidyr")
library("stringr")
library("ggplot2")

##{ Read coastline ---------------------------------------------------------

coast <- read.csv("data/gshhg_medplanet_domain_i.csv")
map <- ggplot() + list(
  geom_polygon(aes(x=lon, y=lat), data=coast, fill="white", colour="grey50", size=0.25),
  coord_quickmap(),
  scale_x_continuous(expand=c(0,0)), scale_y_continuous(expand=c(0,0)),
  theme(
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank()
  )
)
map

# }

# Add zero catches = dates sampled but for which a species is not present
add_zeros <- function(d, id.vars, taxo.vars="species", abund="cpue") {
  # get all date/location sampled
  d1 <- unique(d[,id.vars])
  # get all taxonomic units (i.e. species)
  d2 <- unique(d[,taxo.vars])
  # compute all possibilities
  d1$i <- 0; d2$i <- 0
  all <- left_join(d1, d2, by="i")
  all <- select(all, -i)
  # insert known data into all possibilities and zero-out the rest
  d0 <- left_join(all, d)
  d0[,abund][is.na(d0[,abund])] <- 0
  return(d0)
}


##{ Villefranche -----------------------------------------------------------

d <- read.csv("rade-ichthyo.csv")

# compute average lon and lat of Villefranche
lat <- mean(d$lat, na.rm=TRUE)
lon <- mean(d$lon, na.rm=TRUE)

# cleanup
d <- filter(d, gear == "CARE", dev_stage != "pre-flexion")
d$date <- as.Date(d$collection_date)

# convert to CPUE per capture event
d$cpue <- d$n / d$n_gear
d <- summarise(group_by(d, date, family, species), cpue=sum(cpue))

# add zeros
d <- add_zeros(d, id.vars="date", taxo.vars=c("family", "species"))

# add metadata
d$lon <- lon
d$lat <- lat
d$genus <- str_split_fixed(d$species, " ", 2)[,1]
d$place <- "Villefranche"

# store
vlfr <- d

# }

##{ SUBLIMO ----------------------------------------------------------------

# get metadata
env <- read.csv("SUBLIMO/Environnement 2012_2015.csv")
env <- env[,c("Station", "Date.out")]
env$Date.out <- ymd(env$Date.out)
env$Station <- str_trim(env$Station)
env$Place_code <- str_split_fixed(env$Station, "_", 2)[,1]

# compute effort (number of observations per place and date)
effort <- summarise(group_by(env, Place_code, Date.out), n_care=n())

# get lat and lon for each Place
coords <- read.csv("SUBLIMO/coordinates.csv")
coords$Place_code <- str_split_fixed(coords$Station, "_", 2)[,1]
# compute average lat/lon for each place
coords <- summarise(group_by(coords, Place, Place_code), lat=mean(Latitude), lon=mean(Longitude))

env <- left_join(effort, coords)

# remove two capture events with no location code
env <- na.omit(env)
env <- ungroup(env)


# read captures
# d <- read_excel("SUBLIMO/bdd netoyé 012015.xlsx")
d <- read.csv("SUBLIMO/bdd netoyé 012015.csv", na.strings=c("", "NA"))
# cleanup
d$Family <- str_trim(d$Family)
d$Genus <- str_trim(d$Genus)
d$Species <- str_trim(d$Species)

d$Place  <- str_replace(d$Place, "Port Cros", "Port-Cros")
d$Place  <- str_replace(d$Place, "Saint_Florent", "Saint Florent")

# remove unidentified/empty catches
d <- d[ rowSums(is.na(select(d, Family, Genus, Species))) != 3 , ]
d <- filter(d, Family != "No fish")
# there are still 7 catches with NA abundance... bizarre

# cleanup data
d$Date.out <- ymd(str_c(d$Year, d$Month, d$Day, sep="-"))
d$Species <- str_c(d$Genus, d$Species, sep=" ")
d <- select(d, Place, Date.out, Family, Genus, Species, Abundance)

# remove the problematic abundances
d <- na.omit(d)

# compute cpue
d <- full_join(select(env, -Place_code), d)

colSums(is.na(d))
filter(d, is.na(n_care))


d$cpue <- d$Abundance / d$n_care
d <- filter(d, !is.na(cpue))
d <- summarise(group_by(d, Place, Date.out, Family, Genus, Species), cpue=sum(cpue))
d <- ungroup(d)

# add zeros
d1 <- unique(select(d, Place, Date.out))
d2 <- unique(select(d, Family, Genus, Species))
d1$id <- 0
d2$id <- 0
all <- left_join(d1, d2)
all <- select(all, -id)

d <- left_join(all, d)
d$cpue[is.na(d$cpue)] <- 0

d <- left_join(d, places)

names(d) <- tolower(names(d))
d <- rename(d, date=date.out)

sublimo <- d
# }


##{ Prepare data -----------------------------------------------------------



# collect everything
d <- rbind.fill(sublimo, vlfr)

# add information
d$yday <- yday(d$date)
d$yweek <- week(d$date)
d$week <- week(d$date)
d$year <- year(d$date)
d$julian_day <- as.numeric(difftime(d$date, min(d$date), units="days"))
d$julian_week <- as.numeric(difftime(d$date, min(d$date), units="weeks"))

catches <- summarise(group_by(d, species), n=sum(cpue))
catches <- arrange(catches, desc(n))

map + geom_point(aes(lon, lat, size=sqrt(cpue)), data=d) + scale_size_continuous(range=c(1,20))

# }
save(d, map, catches, file="data.Rdata")
