#-------------------------------------------------------------------#
#                Multi-response Permutation Procedures              #
#                              All Sites                            #
#-------------------------------------------------------------------#

#Research Question: Are the vegetation communities at all sites statistically different? If so, how different?

#Objectives:
#Perform MRPP analysis on vegetation communities at all sites to show they're all different.

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)
library(goeveg)

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

#Reformat Year column to be full year
Quadrats$Year <- year(Quadrats$Date)

#Change column names so they're not so goofy.
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset all sites
Quadrats1234 <- Quadrats %>%
  select(Date, Site, Species) %>%
  group_by(Date, Site) %>%
  

sapply(Quadrats, function(x) unique(x))

Quadrats123GR <- Quadrats %>%
  filter(Year <= 2016) %>%
  filter(Site == "Greving")

Quadrats123PE <- Quadrats %>%
  filter(Year <= 2016) %>%
  filter(Site == "Peckumn")

#Determine what the blooming species are for Greving and Peckumn by date, while retaining site names and dates
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

#Export data file
#write.csv(bsquadrats123GRPEwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/MRPP/Greving and Peckumn Blooming Species")

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

#Export data file 
#write.csv(averageveg123GRPE, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/MRPP/Greving and Peckumn Average Vegetation Coverage")

#Convert to data.frames
averageveg123GRPE <- as.data.frame(averageveg123GRPE)
bsquadrats123GRPEwide <- as.data.frame(bsquadrats123GRPEwide)

#MRPP analysis
GRPE.MRPP <- mrpp(bsquadrats123GRPEwide, averageveg123GRPE$Site, distance = "bray")
GRPE.MRPP

#Use metaMDS in vegan to create a "dissimilarity matrix" to measure similarity between samples; use k = 2 to denote the number of dimensions we're reducing to
GRPE.mds <- metaMDS(comm = bsquadrats123GRPEwide,
                    autotransform = FALSE,
                    k = 2,
                    trymax = 500)

#Check to make sure we're using the correct number of dimensions; 2 is good
dimcheckMDS(bsquadrats123GRPEwide)

#Check stress value; if over 0.2 will have to figure out something else
#Stress value provides a measure of the degree to which the distance between samples in reduced dimensional space corresponds to the actual multivariate distance between the samples. Lower stress values indicate greater conformity.
GRPE.mds$stress
#We're good!

#Plot it
ordiplot(GRPE.mds)
ordihull(GRPE.mds,
         groups = averageveg123GRPE$Site,
         label = TRUE)
#We've got overlapping communities! Yay!