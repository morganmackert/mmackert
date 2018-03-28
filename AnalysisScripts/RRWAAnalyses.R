#-------------------------------------------------------------------#
#                           RRWA Analyses                           #
#                              Year 4                               #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)
library(vegan)

#Read in data
BeeIDs <- read.csv("Bees/Bee IDs.csv", na.strings=c("", "NA"))
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
#They look good!

#Subset BeeIDs to include only 2017 data and appropriate data
BeeIDs4 <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial))

#Determine total number of bee species
BeeIDs4 %>%
  group_by(Binomial) %>%
  summarise()

#Determine total number of individuals for each species
BeeIDs4sppabundance <- BeeIDs4 %>%
  group_by(Binomial) %>%
  count()

#Determine bee abundance by site
BeeIDs4bysite <- BeeIDs4 %>%
  group_by(Site, Binomial) %>%
  count()
BeeIDs4bysite2 <- BeeIDs4bysite %>%
  group_by(Site) %>%
  summarise(Total.Abundance = sum(n))

#Determine species richness by site
BeeIDs4spp <- BeeIDs4 %>%
  group_by(Site) %>%
  summarise(Total.Species = length(unique(Binomial)))

#-------------------------------------------------------------------#
#                       Chao1 Richness Estimate                     #
#-------------------------------------------------------------------#
#Create a table showing number of individuals of each species collected at each site
BeeIDs4indspecSD <- BeeIDs4 %>%
  group_by(Date, Site) %>%
  count(Binomial)
BeeIDs4spec <- BeeIDs4indspecSD %>%
  group_by(Site, Binomial) %>%
  summarise(n = sum(n))

#Reformat from long to wide
BeeIDS4specwide <- spread(BeeIDs4spec, Binomial, n)

#Convert to a dataframe
BeeIDS4specwide <- as.data.frame(BeeIDS4specwide)

#Fill NAs with 0
BeeIDS4specwide[is.na(BeeIDS4specwide)] <- 0

#Change row names to site name
BeeIDS4specwide <- BeeIDS4specwide %>%
  remove_rownames %>%
  column_to_rownames("Site")

#Estimate Chao1 and ACE richness
Chao1 <- estimateR(BeeIDS4specwide)

#-------------------------------------------------------------------#
#                Inverse Simpson's Diversity Index                  #
#-------------------------------------------------------------------#
#Estimate Inverse Simpson's Diversity Index
InvSimp <- diversity(BeeIDS4specwide, "inv")

#Move rownames to "Site" variable
InvSimp <- as.data.frame(InvSimp)
InvSimp <- rownames_to_column(InvSimp, var = "Site")
