#-------------------------------------------------------------------#
#                         Bee Species by County                     #
#                                 2017                              #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(lubridate)
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

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
BeeIDs %>%
  group_by(Site) %>%
  summarise()
#They look good!

#Subset BeeIDs to include only 2017 data and appropriate data
BeeIDs2017 <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial))

#Apply county names to corresponding sites (ugly but it works)
BeeIDs2017 <- BeeIDs2017 %>%
  mutate(County = ifelse(Site == "Plunkett", "Story",
                ifelse(Site == "Bowman", "Dallas",
                       ifelse(Site == "Kaldenberg", "Jasper",
                              ifelse(Site == "McClellan", "Jasper",
                                     ifelse(Site == "Sloan", "Buchanan",
                                            ifelse(Site == "Sheller", "Grundy",
                                                   ifelse(Site == "Cretsinger", "Guthrie",
                                                          ifelse(Site == "Peckumn", "Greene")
                                                          ))))))))

#Determine number of species and numbers of individuals in each county
BeeIDs2017bycounty <- BeeIDs2017 %>%
  group_by(County) %>%
  count(Binomial)

#Export output as .csv file
write.csv(BeeIDs2017bycounty, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/2017 Bee Species by County.csv")

write.csv(Spearman123cor, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SpearmanRank/SpearmanRank123.csv")
