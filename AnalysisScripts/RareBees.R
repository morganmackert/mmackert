#-------------------------------------------------------------------#
#                    Number of Rare Bee Species                     #
#                            Years 1-3                              #
#-------------------------------------------------------------------#

#Research Question: How many of the bee species collected can be classified as "rare," meaning represented by less than ten individuals? How many species were represented by one individual?

#Objectives:
#Determine number of rare bee species collected

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

#Use lubridate to allow R to recognize dates
BeeIDs$Date <- mdy(BeeIDs$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)

#Subset only years 1-3 without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Create table showing number of individuals of each species collected at each site on each date
BeeIDs123indspecSD <- BeeIDs123 %>%
  group_by(Date, Site) %>%
  count(Binomial)

#Use BeeIDS123SDtable to determine number of species collected at each site
BeeIDs123specbysite <- BeeIDs123indspecSD %>%
  group_by(Site) %>%
  summarise(NumberSpecies = length(unique(Binomial)))

#Create table showing number of individuals of each species collected
BeeIDs123indspec <- BeeIDs123 %>%
  count(Binomial)

#Create table showing number of individual bees collected at each site
BeeIDs123indbysite <- BeeIDs123indspecSD %>%
  group_by(Site) %>%
  summarise(NumberIndividuals = sum(n))

#Create a table showing number of individuals of each species by site
BeeIDs123spec <- BeeIDs123indspecSD %>%
  group_by(Site, Binomial) %>%
  summarise(n = sum(n))

#Filter BeeIDs123spec to include only rare species (n < 11)
BeeIDs123rare <- BeeIDs123spec %>%
  filter(n <= 10) %>%
  summarise(NumberRare = length(unique(Binomial)))
  
#Filter BeeIDs123spec to include only singleton species (n = 1)
BeeIDs123single <- BeeIDs123spec%>%
  filter(n == 1) %>%
  summarise(NumberSingletons = length(unique(Binomial)))

#Determine significance levels
BeeIDs123indspecSDAOV <- aov(n ~ Site, data = BeeIDs123indspecSD)
summary(BeeIDs123indspecSDAOV)
BeeIDs123indspecSDLetters <- data.frame("Letters" = multcompLetters(extract_p(TukeyHSD(BeeIDs123indspecSDAOV)$"Site"))$"Letters")

AOV <- aov()
