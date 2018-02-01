#-------------------------------------------------------------------#
#                Multi-response Permutation Procedures              #
#                         Greving and Peckumn                       #
#-------------------------------------------------------------------#

#Research Question: Are the vegetation communities at Greving and Peckumn statistically similar? Is it appropriate to replace Greving with Peckumn?

#Objectives:
#Perform MRPP analysis on vegetation communities at Greving and Peckumn to show Peckumn is similar to Greving

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"), stringsAsFactors = TRUE)
#Date = Date of sample
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#Sample; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Site = Site name
#Quadrat = Quadrat number; 1-10
#Species = Name of plant(s) in quadrat
#X..Cover = Percent coverage of each species within quadrat
#X..Bare.Ground = Percent coverage of bare ground within quadrat
#Species.in.Strip...Not.in.Quadrats = Blooming plant species occurring within the study strip, but not detected within the quadrats
#Outside.Species = Blooming plant species occurring elsewhere on the property

#Use lubridate to allow R to recognize the dates
Quadrats$Date <- mdy(Quadrats$Date)

#Reformat Year column to be full year
Quadrats$Year <- year(Quadrats$Date)

#Change column names so they're not so goofy.
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset only years 1-3 and Greving/Peckumn
Quadrats123GR <- Quadrats %>%
  filter(Year <= 2016) %>%
  filter(Site == "Greving")

Quadrats123PE <- Quadrats %>%
  filter(Year <= 2016) %>%
  filter(Site == "Peckumn")

#Determine number of unique blooming species found in quadrats at each site, not including NAs
numbsquadrats123GR <- Quadrats123GR %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

numbsquadrats123PE <- Quadrats123PE %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

#Determine what the blooming species are for Greving and Peckumn by date, while retaining site names
bsquadrats123GR <- Quadrats123GR %>%
  group_by(Date, Site) %>%
  filter(!is.na(Species)) %>%
  count(Species)

bsquadrats123PE <- Quadrats123PE %>%
  group_by(Date, Site) %>%
  filter(!is.na(Species)) %>%
  count(Species)

#Join Greving and Peckumn data frames
bsquadrats123GRPE <- full_join(bsquadrats123GR, bsquadrats123PE, by = c("Species", "Site", "Date", "n"))

#Reformat from long to wide
bsquadrats123GRPEwide <- spread(bsquadrats123GRPE, Species, n)

#Fill NAs with 0
bsquadrats123GRPEwide[is.na(bsquadrats123GRPEwide)] <- 0

#Export data file for Mary
write.csv(bsquadrats123GRPEwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/MRPP/Greving and Peckumn Blooming Species")

#Remove "Date" and "Site" columns from bsquadrats123GRPEwide data frame
bsquadrats123GRPEwide <- bsquadrats123GRPEwide[!names(bsquadrats123GRPEwide) %in% c("Date", "Site")]

#Calculate average vegetation coverage
averageveg123GR <- Quadrats123GR %>%
  group_by(Date, Site) %>%
  filter(!is.na(Species)) %>%
  summarise(AverageVeg = mean(Cover))

averageveg123PE <- Quadrats123PE %>%
  group_by(Date, Site) %>%
  filter(!is.na(Species)) %>%
  summarise(AverageVeg = mean(Cover))

#Join Greving and Peckumn data frames
averageveg123GRPE <- full_join(averageveg123GR, averageveg123PE, by = c("Site", "Date", "AverageVeg"))

#Export data file for Mary
write.csv(averageveg123GRPE, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/MRPP/Greving and Peckumn Average Vegetation Coverage")

#Convert to data.frames
averageveg123GRPE <- as.data.frame(averageveg123GRPE)
bsquadrats123GRPEwide <- as.data.frame(bsquadrats123GRPEwide)

#MRPP analysis
GRPE.MRPP <- mrpp(bsquadrats123GRPEwide, averageveg123GRPE$Site, distance = "bray")
GRPE.MRPP
