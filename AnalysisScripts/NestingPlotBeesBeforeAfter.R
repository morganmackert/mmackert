#-------------------------------------------------------------------#
#         Bee Species Richness Before and After Nesting Plots       #
#                            Years  3-4                             #
#-------------------------------------------------------------------#

#Research question:  Does bee species richness differ significantly between pre- and post-nesting plot installation?

#Objective:  Perform a t-test on bee species richness during 2016 and 2017 to determine any effect from nesting plot establishment.

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)

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

#Subset only 2016 bees, not including wasps or unidentifiable specimens
BeeIDs3 <- BeeIDs %>%
  filter(Year == 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Subset only 2017 bees, not including wasps or unidentifiable specimens
BeeIDs4 <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Determine number of bee species collected at each site and date for 2016
BeeIDs3bysitedate <- BeeIDs3 %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Determine number of bee species collected at each site and date for 2017
BeeIDs4bysitedate <- BeeIDs4 %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Use t.test to compare means between 2016 and 2017 bee species richness
t.test(BeeIDs3bysitedate$Total.Species, BeeIDs4bysitedate$Total.Species)
