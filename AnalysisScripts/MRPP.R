#-------------------------------------------------------------------#
#                Multi-response Permutation Procedures              #
#                         Greving and Peckumn                       #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(vegan)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"))
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

#Change column names so they're not so goofy.
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset only years 1-3 and Greving/Peckumn
Quadrats123GR <- Quadrats %>%
  filter(Year <= 3) %>%
  filter(Site == "Greving")
Quadrats123PE <- Quadrats %>%
  filter(Year <= 3) %>%
  filter(Site == "Peckumn")

#Determine number of unique blooming species found in quadrats at each site, not including NAs
numbsquadrats123GR <- Quadrats123GR %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

numbsquadrats123PE <- Quadrats123PE %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

#Determine what the blooming species are
bsquadrats123GR <- Quadrats123GR %>%
  filter(!is.na(Species)) %>%
  count(Species)

bsquadrats123PE <- Quadrats123PE %>%
  filter(!is.na(Species)) %>%
  count(Species)

#Calculate average vegetation coverage
averageveg123GR <- Quadrats123GR %>%
  filter(!is.na(Species)) %>%
  summarise(AverageVeg = mean(Cover))

averageveg123PE <- Quadrats123PE %>%
  filter(!is.na(Species)) %>%
  summarise(AverageVeg = mean(Cover))

#MRPP analysis
GRPE.MRPP <- mrpp(bsquadrats123GR, bsquadrats123PE)


data(dune)
data(dune.env)
dune.mrpp <- mrpp(dune, dune.env$Management)
dune.mrpp




















