#-------------------------------------------------------------------#
#                       Guild Chi-Squared Test                      #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

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
BeeIDs <- read.csv("Bees/Bee IDs.csv")
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet
Fulldata <- read.csv("Combined full data set.csv")
#Date = Date of sample
#Site = Site name
#Sampling.Period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#X..Floral.Cover..in.10m2. = Average coverage of blooming forb/weed species in ten quadrats
#X..Blooming.species.in.quadrats = Number of forb/weed species in bloom within ten quadrats
#X..Bare.Ground..in.10m2. = Average bare ground coverage in ten quadrats
#Trapname.Abundance = Number of individual bees collected by specified trap/site/date
#Total.Abundance = Number of individual bees collected by all trap types at the specified site/date
#Trapname.Species.Richness = Number of bee species collected by specified trap/site/date
#Total.Species.Richness = Number of bee species collected by all trap types at the specified site/date
#Species.Name = Number of individuals of specified species collected at the specified site/date

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)
Fulldata$Date <- mdy(Fulldata$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
BeeIDs %>%
  group_by(Site) %>%
  summarise()

#Same with "Trap"
BeeIDs %>%
  group_by(Trap) %>%
  summarise()

#We find that site names are good to go, but trap names need some work!
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

years123 <- Fulldata %>%
  filter(Year <= 3)

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
