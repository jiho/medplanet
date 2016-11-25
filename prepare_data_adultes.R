library("readxl")
library("stringr")
library("plyr")
library("dplyr")
library("lubridate")
library("ggplot2")



## Read Embiez ----

a <- read_excel("data/adultes/données adultes Embiez.xlsx", sheet=1, skip=1)

# clean names written in files
a$family <- str_trim(tolower(a$family))
a$genus <- str_trim(tolower(a$genus))
a$species <- str_trim(tolower(a$species))

# Join genus and species suffix in species column
a$species <- str_c(a$genus, " ", a$species)

# check that each genus has the same family
filter(summarise(group_by(a, genus), n=length(unique(family))), n>1)
filter(summarise(group_by(a, species), n=length(unique(genus))), n>1)

save(a, file="donees adultes.rda")


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

# remove non-fish
l <- l[-which(l$family == "octopodidae"),]
l <- l[-which(l$family == "sepiidae"),]
l <- l[-which(l$family == "sepiolidae"),]
l <- l[-which(l$family == "argonautidae"),]
l <- l[-which(l$family == "loliginidae"),]

# remove non-coastal fish
l <- l[-which(l$life == "Pélagique"& l$coastal == "Non"& l$`coastal juveniles` == "Non"),]
l <- l[-which(l$life == "Benthique"& l$coastal == "Non"& l$`coastal juveniles` == "Non"),]

#l <- l[-which(l$family == "engraulidae"),]
#l <- l[-which(l$family == "scombridae"),]
l <- l[-which(l$family == "clupeidae"),]
l <- l[-which(l$family == "sphyraenidae"),]

#rename family
l$family[which(l$family == "lotidae")] <- "gadidae"


save(l, file="listesp.rda")

