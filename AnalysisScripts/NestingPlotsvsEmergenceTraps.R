#-------------------------------------------------------------------#
#      Bee Species Richness: Nesting Plots vs. Emergence Traps      #
#                              Year 4                               #
#-------------------------------------------------------------------#

#Research question:  How does bee species richness differ between bees collected from the nesting plots and emergence traps?

#Objective:  Perform a t-test on bee species richness between nesting plots and emergence traps.

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

#Subset BeeIDs to include only bees collected from nesting plots in 2017, not including wasps or unidentifiable specimens
BeeIDs4NP <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap == "Plot") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Subset BeeIDs to include only bees collected from emergence traps in 2017, not including wasps or unidentifiable specimens
BeeIDs4ET <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap == "Emergence") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Determine number of species collected at each site and date in nesting plots
BeeIDs4NPbysitedate <- BeeIDs4NP %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Determine number of species collected at each site and date in emergence traps
BeeIDs4ETbysitedate <- BeeIDs4ET %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Use t.test to compare means between nesting plots and emergence traps
t.test(BeeIDs4NPbysitedate$Total.Species, BeeIDs4ETbysitedate$Total.Species, var.equal = FALSE)
