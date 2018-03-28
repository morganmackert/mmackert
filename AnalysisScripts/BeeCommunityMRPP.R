#-------------------------------------------------------------------#
#                Multi-response Permutation Procedures              #
#                          Bee Communities                          #
#-------------------------------------------------------------------#

#Research Question: Do the bee communities at sites with varying blooming forb/weed diversities differ significantly from one another?

#Objectives:
#Perform MRPP analysis on bee species communities at each site as a function of the number of unique blooming plant species present

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)
library(MASS)

#Read in data
BeeIDs <- read.csv("Data/Bees/Bee IDs.csv")
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

#-------------------------------------------------------------------#
#                             2014-2016                             #
#-------------------------------------------------------------------#
#Subset only years 1-3; BeeIDs without target/pitfall bees, bees that were collected during times when quadrats weren't conducted, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(!is.na(Binomial)) %>%
  filter(Date != "2014-07-09") %>%
  filter(Date != "2014-08-12") %>%
  filter(Date != "2015-06-13") %>%
  filter(Date != "2015-06-10") %>%
  filter(Date != "2015-08-11") %>%
  filter(Trap != "Target") %>%
  filter(Trap != "Pitfall") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Determine number of individuals per bee species collected at each site
BeeIDs123bysite <- BeeIDs123 %>%
  group_by(Date, Site) %>%
  count(Binomial)

#Reformat from long to wide
BeeIDs123bysitewide <- spread(BeeIDs123bysite, Binomial, n)

#Fill NAs with 0
BeeIDs123bysitewide[is.na(BeeIDs123bysitewide)] <- 0

#Move "Site" column from BeeIDs123bysitewide to another data frame
BeeIDs123bysitewidesites <- BeeIDs123bysitewide["Site"]

#Remove "Site" and "Date" columns
BeeIDs123bysitewide <- BeeIDs123bysitewide[!names(BeeIDs123bysitewide) %in% c("Site", "Date")]

#Convert to data.frame
BeeIDs123bysitewide <- as.data.frame(BeeIDs123bysitewide)

#Perform MRPP analysis
BeeCommunityMRPP <- mrpp(BeeIDs123bysitewide, BeeIDs123bysitewidesites$Site, distance = "bray")
BeeCommunityMRPP

#Read in BeeCommunityMRPP results to graph
MRPP <- read.csv("mmackert/Graphs/BeeCommunityMRPP/Bee Community by Site MRPP Results.csv")

#Graph results
BeeCommunityMRPPplot <- ggplot(MRPP,
                               aes(x = Site,
                                   y = Delta)) +
  geom_point(size = 3) +
  theme_bw() +
  labs(y = "Δ")
  #scale_x_discrete(limits = c("6", "1", "10", "3", "4", "11", "5", "7", "9", "2", "8"))
BeeCommunityMRPPplot

#-------------------------------------------------------------------#
#                               2017                                #
#-------------------------------------------------------------------#
#Subset only years 1-3; BeeIDs without target/pitfall bees, bees that were collected during times when quadrats weren't conducted, wasps, or unidentifiable specimens
BeeIDs4 <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(!is.na(Binomial)) %>%
  filter(Trap != "Target") %>%
  filter(Trap != "Pitfall") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Determine number of individuals per bee species collected at each site
BeeIDs4bysite <- BeeIDs4 %>%
  group_by(Date, Site) %>%
  count(Binomial)

#Reformat from long to wide
BeeIDs4bysitewide <- spread(BeeIDs4bysite, Binomial, n)

#Fill NAs with 0
BeeIDs4bysitewide[is.na(BeeIDs4bysitewide)] <- 0

#Move "Site" column from BeeIDs4bysitewide to another data frame
BeeIDs4bysitewidesites <- BeeIDs4bysitewide["Site"]

#Remove "Site" and "Date" columns
BeeIDs4bysitewide <- BeeIDs4bysitewide[!names(BeeIDs4bysitewide) %in% c("Site", "Date")]

#Convert to data.frame
BeeIDs4bysitewide <- as.data.frame(BeeIDs4bysitewide)

#Perform MRPP analysis
BeeCommunityMRPP4 <- mrpp(BeeIDs4bysitewide, BeeIDs4bysitewidesites$Site, distance = "bray")
BeeCommunityMRPP4

#Read in BeeCommunityMRPP results to graph
MRPP <- read.csv("mmackert/Graphs/BeeCommunityMRPP/Bee Community by Site MRPP Results.csv")

#Graph results
BeeCommunityMRPPplot <- ggplot(MRPP,
                               aes(x = Site,
                                   y = Delta)) +
  geom_point(size = 3) +
  theme_bw() +
  labs(y = "Δ")
#scale_x_discrete(limits = c("6", "1", "10", "3", "4", "11", "5", "7", "9", "2", "8"))
BeeCommunityMRPPplot