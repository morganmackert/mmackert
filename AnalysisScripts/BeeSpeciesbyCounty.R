#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                                 2017                              #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

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

#Apply county names to corresponding sites (ugly but it works)
BeeIDs4 <- BeeIDs4 %>%
  mutate(County = ifelse(Site == "Plunkett", "Story",
                ifelse(Site == "Bowman", "Dallas",
                       ifelse(Site == "Kaldenberg", "Jasper",
                              ifelse(Site == "McClellan", "Jasper",
                                     ifelse(Site == "Sloan", "Buchanan",
                                            ifelse(Site == "Sheller", "Grundy",
                                                   ifelse(Site == "Cretsinger", "Guthrie",
                                                          ifelse(Site == "Peckumn", "Greene",
                                                                 NA
                                                          )))))))))

#Check to make sure mutate function worked
BeeIDs4 %>%
  group_by(Site, County) %>%
  summarise()
#BOOM

#Determine number of species and numbers of individuals in each county
BeeIDs4bycounty <- BeeIDs4 %>%
  group_by(County) %>%
  count(Binomial)

#Export output as .csv file
#write.csv(BeeIDs4bycounty, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/2017 Bee Species by County.csv")

#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                               2014-2017                           #
#-------------------------------------------------------------------#

#Subset BeeIDs to include only 2014-2017 dates and appropriate data
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017) %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Determine total number of bee species
BeeIDs1234species <- BeeIDs1234 %>%
  group_by(Binomial) %>%
  summarise()

#Determine number of bee species by site
BeeIDs1234speciesbysite <- BeeIDs1234 %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Determine number of bees collected at each site
BeeIDs1234abundance <- BeeIDs1234 %>%
  group_by(Site, Date) %>%
  count()

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

#Check to make sure mutate function worked
BeeIDs1234 %>%
  group_by(Site, County) %>%
  summarise()

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

#Determine number of species and numbers of individuals in each county
BeeIDs1234bycounty <- BeeIDs1234 %>%
  group_by(County) %>%
  count(Binomial)

#Determine number of bees collected in RRW and outside
BeeIDs1234RRW <- BeeIDs1234 %>%
  group_by(RRW) %>%
  count(Binomial) %>%
  summarise(Total.Bees = sum(n))

#Determine number of bee species collected in RRW and outside
BeeIDs1234RRWspecies <- BeeIDs1234 %>%
  group_by(RRW) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Which species caught in which locations?
BeeIDs1234RRWunique <- BeeIDs1234 %>%
  group_by(RRW) %>%
  count(Binomial)

#Export as .csv
#write.csv(BeeIDs1234RRWunique, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/RRW Bees.csv")