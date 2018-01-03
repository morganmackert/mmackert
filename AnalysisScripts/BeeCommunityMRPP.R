#-------------------------------------------------------------------#
#                Multi-response Permutation Procedures              #
#                          Bee Communities                          #
#-------------------------------------------------------------------#

#Research Question: Do the bee communities at sites with varying blooming forb/weed diversities differ significantly from one another?

#Objectives:
#Perform MRPP analysis on bee species communities at each site as a function of the number of unique blooming plant species present

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)

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
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"), stringsAsFactors = TRUE)
#Date = Date of sample
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#Sample; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Site = Site name
#Quadrat = Quadrat number; 1-10
#Species = Name of plant(s) in quadrat
#X..Cover = Percent coverage of each species within quadrat
#X..Bare.Ground = Percent coverage of bare ground within quadrat
#Species.in.Strip...Not.in.Quadrats = Blooming plant species occurring within the study strip, but not detected within the quadrats
#Outside.Species = Blooming plant species occurring elsewhere on the property

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)
Quadrats$Year <- year(Quadrats$Date)

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
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(!is.na(Binomial)) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

Quadrats123 <- Quadrats %>%
  filter(Year <= 2016)

#Determine number of individuals per bee species collected at each site
BeeIDs123bysite <- BeeIDs123 %>%
  group_by(Date, Site) %>%
  count(Binomial)

#Reformat from long to wide
BeeIDs123bysitewide <- spread(BeeIDs123bysite, Binomial, n)

#Fill NAs with 0
BeeIDs123bysitewide[is.na(BeeIDs123bysitewide)] <- 0

#Remove "Site" and "Date" columns
BeeIDs123bysitewide <- BeeIDs123bysitewide[!names(BeeIDs123bysitewide) %in% c("Site", "Date")]

#Determine number of unique blooming species found in quadrats at each site, not including NAs
bsquadrats123 <- Quadrats123 %>%
  group_by(Date, Site) %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

#Convert to data.frames
BeeIDs123bysitewide <- as.data.frame(BeeIDs123bysitewide)
bsquadrats123 <- as.data.frame(bsquadrats123)

#Perform MRPP analysis
BeeCommunityMRPP <- mrpp(BeeIDs123bysitewide, bsquadrats123$Site, distance = "bray")
BeeCommunityMRPP

















