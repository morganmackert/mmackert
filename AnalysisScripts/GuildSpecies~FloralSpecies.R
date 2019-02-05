#-------------------------------------------------------------------#
#         Guild Species Richness ~ Floral Species Richness          #
#-------------------------------------------------------------------#

#Research Question: Does the number of bee species in each nesting guild vary with increasing numbers of floral species in bloom?

#Objective:  Create a model and graph showing the relationship between guild species richness and floral species richness

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = TRUE, na.strings = c("", "NA"))
Quadrats <- read.csv("Plants/Quadrats.csv", header = TRUE)

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Subset BeeIDs without target bees, wasps, or unidentifiable specimens
bees <- Bees %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site))

#Assign guild name to each specimen
bees <- bees %>%
  mutate(Guild = case_when(
    Binomial == "Bombus citrinus" ~ "Social parasite",
    Binomial == "Megachile latimanus" ~ "Solitary ground-nester",
    Genus == "Agapostemon" | Genus == "Andrena" | Genus == "Calliopsis" | Genus == "Colletes" | Genus == "Dieunomia" | Genus == "Melissodes" | Genus == "Pseudopanurgus" | Genus == "Perdita" | Genus == "Lasioglossum" | Genus == "Eucera" | Genus == "Anthophora" | Genus == "Svastra" | Genus == "Nomia" | Genus == "Florilegus" | Genus == "Peponapis" | Genus == "Dufourea" | Genus == "Protandrena" | Genus == "Lasioglossum (Evylaeus)" ~ "Solitary ground-nester",
    Genus == "Lasioglossum (Dialictus)" | Genus == "Augochlorella" | Genus == "Augochlora" | Genus == "Halictus" | Genus == "Augochloropsis" ~ "Social ground-nester",
    Genus == "Apis" ~ "Honey bee",
    Genus == "Bombus" ~ "Bumble bee",
    Genus == "Heriades" | Genus == "Hoplitis" | Genus == "Hylaeus" | Genus == "Megachile" | Genus == "Osmia" | Genus == "Ceratina" | Binomial == "Anthophora terminalis" | Genus == "Xylocopa" | Genus == "Ashmeadiella" ~ "Cavity nester",
    Genus == "Coelioxys" | Genus == "Holcopasites" | Genus == "Nomada" | Genus == "Sphecodes" | Genus == "Stelis" | Genus == "Triepeolus" | Genus == "Xeromelecta" | Genus == "Epeolus" ~ "Cleptoparasite"
  ))

#Check to make sure all species have been assigned a guild
bees %>% 
  group_by(Guild) %>% 
  tally()
#Good!

#Determine number of bee species per guild for each site/date
bees.guild <- bees %>%
  group_by(Site, Date, Guild) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Determine number of plant species in bloom for each site/date
floral.species <- Quadrats %>%
  group_by(Site, Date) %>%
  summarise(no.floralspp = n_distinct(Species, na.rm = TRUE))

#Join the two datasets together
floralspp.beeguild <- full_join(bees.guild, floral.species, by = c("Site", "Date"))

#Fill NAs with 0
floralspp.beeguild$no.beespp[is.na(floralspp.beeguild$no.beespp)] <- 0
floralspp.beeguild$no.floralspp[is.na(floralspp.beeguild$no.floralspp)] <- 0

#Add Year column
floralspp.beeguild$Year <- year(floralspp.beeguild$Date)


