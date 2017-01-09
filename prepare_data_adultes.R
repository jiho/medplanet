library("readxl")
library("stringr")
library("plyr")
library("dplyr")
library("lubridate")
library("ggplot2")



#Read species list of north west mediterranean
l <- read_excel("data/Liste especes mediterranee.xlsx", sheet=1)

# clean names written in files
l$family <- str_trim(tolower(l$family))
l$genus <- str_trim(tolower(l$genus))
l$species <- str_trim(tolower(l$species))

# remove indetermined species (1 atherinidae sp.)
l<- l[-which(l$species=="sp."),]

# Join genus and species suffix in species column
l$species <- str_c(l$genus, " ", l$species)

# check that each genus has the same family
filter(summarise(group_by(l, genus), n=length(unique(family))), n>1)
filter(summarise(group_by(l, species), n=length(unique(genus))), n>1)

#rename family
l$family[which(l$family == "lotidae")] <- "gadidae"

# remove non-coastal fish
louisy <- l

l <- l[-which(l$family == "engraulidae"),] # anchois
l <- l[-which(l$family == "scombridae"),]  #thons...
l <- l[-which(l$family == "clupeidae"),]  # sardines
l <- l[-which(l$family == "sphyraenidae"),]  #barracudas, car JO l'a mis dans son script
l <- l[-which(l$family == "exocoetidae"),] #poissons volants
l <- l[-which(l$family == "coryphaenidae"),] # coryphène
l <- l[-which(l$family == "chlorophthalmidae"),] #
l <- l[-which(l$family == "xiphiidae"),] # espadons
l <- l[-which(l$family == "istiophoridae"),] # marlin
l <- l[-which(l$family == "trichiuridae"),]#sabres
l <- l[-which(l$family == "molidae"),]#poisson lune
l <- l[-which(l$family == "regalecidae"),] #regalec

l <- l[-which(l$family == "trachipteridae"),]
l <- l[-which(l$family == "ammodytidae"),]
l <- l[-which(l$family == "haemulidae"),]
l <- l[-which(l$family == "macrouridae"),]
l <- l[-which(l$family == "aulopidae"),]
l <- l[-which(l$family == "petromyzonidae"),]

save(l,louisy, file="listesp.rda")

