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
