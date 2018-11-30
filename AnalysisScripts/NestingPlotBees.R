#-------------------------------------------------------------------#
#                          Nesting Plot Bees                        #
#-------------------------------------------------------------------#

#Research Question:  If we provide bare soil areas within flight distance of strips, will ground-nesting bees utilize them?
#Objectives:  Quantify the number of individual bees and bee species collected from the nesting plots

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv")

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)

#Add new column with only the year
Bees$Year <- year(Bees$Date)

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
Bees %>%
  group_by(Site) %>%
  summarise()

#Same with "Trap"
Bees %>%
  group_by(Trap) %>%
  summarise()

#Year 4 ####
#-------------------------------------------------------------------#
#                               Year 4                              #
#-------------------------------------------------------------------#

#Subset only 2017 bees, not including wasps or unidentifiable specimens
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017) %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Apply county names to corresponding sites (ugly but it works)
BeeIDs1234 <- BeeIDs1234 %>%
  mutate(County = ifelse(Site == "Plunkett", "Story",
                         ifelse(Site == "Bowman", "Dallas",
                                ifelse(Site == "Kaldenberg", "Jasper",
                                       ifelse(Site == "McClellan", "Jasper",
                                              ifelse(Site == "Sloan", "Buchanan",
                                                     ifelse(Site == "Sheller", "Grundy",
                                                            ifelse(Site == "Cretsinger", "Guthrie",
                                                                   ifelse(Site == "Peckumn", "Greene",
                                                                          ifelse(Site == "Greving", "Carroll",
                                                                                 ifelse(Site == "NealSmith", "Jasper",
                                                                                        ifelse(Site == "Elkader", "Clayton",
                                                                                               NA
                                                                                        ))))))))))))
#Apply RRW (yes or no) to counties
BeeIDs1234 <- BeeIDs1234 %>%
  mutate(RRW = ifelse(County == "Story", "Non-RRW",
                      ifelse(County == "Dallas", "RRW",
                             ifelse(County == "Jasper", "Non-RRW",
                                    ifelse(County == "Buchanan", "Non-RRW",
                                           ifelse(County == "Grundy", "Non-RRW",
                                                  ifelse(County == "Guthrie", "RRW",
                                                         ifelse(County == "Greene", "RRW",
                                                                ifelse(County == "Carroll", "RRW",
                                                                       ifelse(County == "Clayton", "Non-RRW",
                                                                              NA
                                                                       ))))))))))

#Subset only nesting plot bees
BeeIDs1234NP <- BeeIDs1234 %>%
  filter(Trap == "Plot")

#Determine number of bees collected in RRW and non-RRW sites
BeeIDs1234RRWNP <- BeeIDs1234NP %>%
  group_by(RRW) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site and date
BeeIDs1234NPbysitedate <- BeeIDs1234NP %>%
  group_by(Site, Date) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site
BeeIDs1234NPbysite <- BeeIDs1234NP %>%
  group_by(Site) %>%
  count(Binomial)

#Table describing the number of species collected at each site
BeeIDs1234NPsppbysite <- BeeIDs1234NP %>%
  group_by(Site) %>%
  summarise(Total.Richness = length(unique(Binomial)))

#Year 5 ####
#-------------------------------------------------------------------#
#                               Year 5                              #
#-------------------------------------------------------------------#

#Filter out wasps and unidentifiable specimens
Bees <- Bees %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Subset Bees to include only nesting plot bees
BeesNP <- Bees %>%
  filter(Trap == "Plot")

#Determine total number of species
BeesNPspp <- BeesNP %>%
  group_by(Binomial) %>%
  count()

#Which sites were they collected from?
BeesNPbysite <- BeesNP %>%
  group_by(Site) %>%
  count(Binomial)

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