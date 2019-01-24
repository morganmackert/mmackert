#-------------------------------------------------------------------#
#                       Guild Chi-Squared Test                      #
#-------------------------------------------------------------------#

#Research question:  Do the number of bee species within each nesting guild differ between sites?

#Objective:  Conduct Pearson's chi-squared test on the number of bee species within each nesting guild collected at each site

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(vegan)
library(MASS)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = TRUE, na.strings = c("", "NA"))

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)

#Add new column with only the year
Bees$Year <- year(Bees$Date)

#Get rid of icky stuff in Bees
bees <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site))

#Years 1-3 ####
#-------------------------------------------------------------------#
#                               Years 1-3                           #
#-------------------------------------------------------------------#
#Subset only years 1-3
BeeIDs123 <- bees %>%
  filter(Year <= 2016)

#Assign guild name to each specimen
BeeIDs123 <- BeeIDs123 %>%
  mutate(Guild = case_when(
    Binomial == "Bombus citrinus" ~ "Social parasite",
    Binomial == "Megachile latimanus" ~ "Solitary ground-nester",
    Genus == "Agapostemon" | Genus == "Andrena" | Genus == "Calliopsis" | Genus == "Colletes" | Genus == "Lasioglossum (Lasioglossum)" | Genus == "Melissodes" | Genus == "Pseudopanurgus" | Genus == "Perdita" | Genus == "Lasioglossum s.s." | Genus == "Lasioglossum"| Genus == "Eucera" | Genus == "Lasioglossum (Leuchalictus)" | Genus == "Lasioglossum (Hemihalictus)" | Genus == "Anthophora" | Genus == "Svastra" | Genus == "Nomia" | Genus == "Florilegus" | Genus == "Lasioglossum (Sphecodogastra)" | Genus == "Peponapis" | Genus == "Duforea" ~ "Solitary ground-nester",
    Genus == "Lasioglossum (Dialictus)" | Genus == "Lasioglossum (Evylaeus)" | Genus == "Augochlorella" | Genus == "Augochlora" | Genus == "Halictus" | Genus == "Augochloropsis" ~ "Social ground-nester",
    Genus == "Apis" ~ "Honey bee",
    Genus == "Bombus" ~ "Bumble bee",
    Genus == "Hoplitis" | Genus == "Hylaeus" | Genus == "Megachile" | Genus == "Osmia" | Genus == "Ceratina" | Binomial == "Anthophora terminalis" | Genus == "Xylocopa" | Genus == "Ashmeadiella" ~ "Cavity nester",
    Genus == "Coelioxys" | Genus == "Holcopasites" | Genus == "Nomada" | Genus == "Sphecodes" | Genus == "Triepeolus" ~ "Cleptoparasite"
  ))

#Calculate number of species within each guild at each site
Guildsppbysite123 <- BeeIDs123 %>%
  group_by(Site, Guild) %>%
  filter(!is.na(Binomial)) %>%
  summarise(Species = length(unique(Binomial)))

#What are the species in Guildsppbysite123? Double check.
Guildsppbysite123nums <- BeeIDs123 %>%
  group_by(Site, Guild) %>%
  filter(!is.na(Binomial)) %>%
  count(Binomial)
#Looks good.

#Reformat from long to wide
Guildsppbysite123wide <- spread(Guildsppbysite123, Guild, Species)

#Fill NAs with 0
Guildsppbysite123wide[is.na(Guildsppbysite123wide)] <- 0

#Remove "Site" column from GuildbyVeg123 data frame
Guildsppbysite123wide <- Guildsppbysite123wide[!names(Guildsppbysite123wide) %in% c("Site")]

#Perform Chi-Squared test
chisq.test(Guildsppbysite123wide)

#-------------------------------------------------------------------#
#                   Cleptoparasite Chi-Squared Test                 #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Transform BeeIDs123 to include only cleptoparasites
BeeIDS123CP <- BeeIDs123 %>%
  filter(Guild == "Cleptoparasite")

#Create column denoting 
BeeIDs123 <- BeeIDs123 %>%
  mutate(Guild = case_when(
    Binomial == "Bombus citrinus" ~ "Social parasite",
    Binomial == "Megachile latimanus" ~ "Solitary ground-nester",
    Genus == "Agapostemon" | Genus == "Andrena" | Genus == "Calliopsis" | Genus == "Colletes" | Genus == "Lasioglossum (Lasioglossum)" | Genus == "Melissodes" | Genus == "Pseudopanurgus" | Genus == "Perdita" | Genus == "Lasioglossum s.s." | Genus == "Lasioglossum"| Genus == "Eucera" | Genus == "Lasioglossum (Leuchalictus)" | Genus == "Lasioglossum (Hemihalictus)" | Genus == "Anthophora" | Genus == "Svastra" | Genus == "Nomia" | Genus == "Florilegus" | Genus == "Lasioglossum (Sphecodogastra)" | Genus == "Peponapis" | Genus == "Duforea" ~ "Solitary ground-nester",
    Genus == "Lasioglossum (Dialictus)" | Genus == "Lasioglossum (Evylaeus)" | Genus == "Augochlorella" | Genus == "Augochlora" | Genus == "Halictus" | Genus == "Augochloropsis" ~ "Social ground-nester",
    Genus == "Apis" ~ "Honey bee",
    Genus == "Bombus" ~ "Bumble bee",
    Genus == "Hoplitis" | Genus == "Hylaeus" | Genus == "Megachile" | Genus == "Osmia" | Genus == "Ceratina" | Binomial == "Anthophora terminalis" | Genus == "Xylocopa" | Genus == "Ashmeadiella" ~ "Cavity nester",
    Genus == "Coelioxys" | Genus == "Holcopasites" | Genus == "Nomada" | Genus == "Sphecodes" | Genus == "Triepeolus" ~ "Cleptoparasite"
  ))

#Data dictionary ####
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet
#Old code ####
#We find that site names are good to go, but trap names need some work!
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"