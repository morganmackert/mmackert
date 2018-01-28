#-------------------------------------------------------------------#
#                          Nesting Plot Bees                        #
#                              Year 4                               #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)

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

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)

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

#Subset only 2017 nesting plot bees, not including wasps or unidentifiable specimens
BeeIDs4NP <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap == "Plot") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Table describing the number of individuals per species at each site and date
BeeIDs4NPbysitedate <- BeeIDs4NP %>%
  group_by(Site, Date) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site
BeeIDs4NPbysite <- BeeIDs4NP %>%
  group_by(Site) %>%
  count(Binomial)

#Table describing the number of species collected at each site
BeeIDs4NPsppbysite <- BeeIDs4NP %>%
  group_by(Site) %>%
  summarise(Total.Richness = length(unique(Binomial)))