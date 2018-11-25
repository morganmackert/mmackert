#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#-------------------------------------------------------------------#

#Research Question:  How does the bee species community vary between Iowa counties?

#Objectives:  Determine the number of bee species and the number of individuals in each species collected in each county sampled

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", na.strings=c("", "NA"))

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)

#Add new column with only the year
Bees$Year <- year(Bees$Date)

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
Bees %>%
  group_by(Site) %>%
  summarise()
#They look good!

#Year 4 ####
#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                                 2017                              #
#-------------------------------------------------------------------#

#Subset BeeIDs to include only 2017 data and appropriate data
Bees4 <- Bees %>%
  filter(Year == 2017) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial))

#Determine total number of bee species
Bees4 %>%
  group_by(Binomial) %>%
  summarise()

#Determine total number of individuals for each species
Bees4sppabundance <- Bees4 %>%
  group_by(Binomial) %>%
  count()

#Determine bee abundance by site
Bees4bysite <- Bees4 %>%
  group_by(Site, Binomial) %>%
  count()
Bees4bysite2 <- Bees4bysite %>%
  group_by(Site) %>%
  summarise(Total.Abundance = sum(n))

#Determine species richness by site
Bees4spp <- Bees4 %>%
  group_by(Site) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Apply county names to corresponding sites (ugly but it works)
Bees4 <- Bees4 %>%
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
Bees4 %>%
  group_by(Site, County) %>%
  summarise()
#BOOM

#Determine number of species and numbers of individuals in each county
Bees4bycounty <- Bees4 %>%
  group_by(County) %>%
  count(Binomial)

#Export output as .csv file
#write.csv(BeeIDs4bycounty, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/2017 Bee Species by County.csv")

#Years 1-4 ####
#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                               2014-2017                           #
#-------------------------------------------------------------------#

#Subset Bees to include only 2014-2017 dates and appropriate data
Bees1234 <- Bees %>%
  filter(Year <= 2017) %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Determine total number of bee species
Bees1234species <- Bees1234 %>%
  group_by(Binomial) %>%
  summarise()

#Determine number of bee species by site
Bees1234speciesbysite <- Bees1234 %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Determine number of bees collected at each site
Bees1234abundance <- Bees1234 %>%
  group_by(Site, Date) %>%
  count()

#Apply county names to corresponding sites (ugly but it works)
Bees1234 <- Bees1234 %>%
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
Bees1234 %>%
  group_by(Site, County) %>%
  summarise()

#Apply RRW (yes or no) to counties
Bees1234 <- Bees1234 %>%
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
Bees1234bycounty <- Bees1234 %>%
  group_by(County) %>%
  count(Binomial)

#Determine number of bees collected in RRW and outside
Bees1234RRW <- Bees1234 %>%
  group_by(RRW) %>%
  count(Binomial) %>%
  summarise(Total.Bees = sum(n))

#Determine number of bee species collected in RRW and outside
Bees1234RRWspecies <- Bees1234 %>%
  group_by(RRW) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Which species caught in which locations?
Bees1234RRWunique <- Bees1234 %>%
  group_by(RRW) %>%
  count(Binomial)

#Export as .csv
#write.csv(BeeIDs1234RRWunique, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/RRW Bees.csv")

#Years 4-5 ####
#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                               2017-2018                           #
#-------------------------------------------------------------------#

#Subset Bees to include only 2017-2018 dates and appropriate data
Bees45 <- Bees %>%
  filter(Year >= 2017) %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Determine total number of bee species
Bees45species <- Bees45 %>%
  group_by(Binomial) %>%
  summarise()

#Determine number of bee species by site
Bees45speciesbysite <- Bees45 %>%
  group_by(Site, Date) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Determine number of bees collected at each site
Bees45abundance <- Bees45 %>%
  group_by(Site, Date) %>%
  count()

#Apply county names to corresponding sites (ugly but it works)
Bees45 <- Bees45 %>%
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
Bees45 %>%
  group_by(Site, County) %>%
  summarise()
#G2G

#Determine number of species and numbers of individuals for each year
Bees45byyear <- Bees45 %>%
  group_by(Binomial, Year) %>%
  count(Binomial)

#Separate by Year
Bees45byyear <- Bees45byyear %>%
  spread(Year, n)

#Determine counties each species was collected in
Bees45bycounty <- Bees45 %>%
  group_by(Binomial, County) %>%
  count(Binomial)

#Collapse rows together
Bees45bycounty2 <- Bees45bycounty %>%
  group_by(Binomial) %>%
  summarise(Counties = paste(County, collapse = ", "))

#Join the two data frames together
Bees45byyearcounty <- full_join(Bees45byyear, Bees45bycounty2, by = "Binomial")

#Export as .csv
write.csv(Bees45byyearcounty, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/2017-2018 Bee Species by County.csv")

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