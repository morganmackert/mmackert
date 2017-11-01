#-------------------------------------------------------------------#
#                   Sampling Effort ~ Bee Species                   #
#                              Years 1-3                            #
#-------------------------------------------------------------------#

#Research Question: How many sampling events are required to reach bee species saturation?

#Objectives:
#Create model(s) to explore relationship between sampling effort and the number of bee species collected as a result
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(tibble)
library(dplyr)
library(vegan)

#Read in data
Fulldata <- read.csv("Data/Combined full data set.csv")
#Date = Date of sample
#Site = Site name
#Sampling.Period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#X..Floral.Cover..in.10m2. = Average coverage of blooming forb/weed species in ten quadrats
#X..Blooming.species.in.quadrats = Number of forb/weed species in bloom within ten quadrats
#X..Bare.Ground..in.10m2. = Average bare ground coverage in ten quadrats
#Trapname.Abundance = Number of individual bees collected by specified trap/site/date
#Total.Abundance = Number of individual bees collected by all trap types at the specified site/date
#Trapname.Species.Richness = Number of bee species collected by specified trap/site/date
#Total.Species.Richness = Number of bee species collected by all trap types at the specified site/date
#Species.Name = Number of individuals of specified species collected at the specified site/date

#Remove unnecessary columns
Fulldata <- Fulldata[-c(1, 3, 5:22)]

#-------------------------------------------------------------------#
#                              Plunkett                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Plunkett samples
PL123 <- Fulldata %>%
  filter(Site == "Plunkett") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
PL123$Sample <- 1:14

#Site and Year columns no longer necessary
PL123 <- PL123[-c(1:2)]

#Change row names to assigned sample number
PL123 <- PL123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Plunkett
rarecurve(PL123)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Bowman samples
BO123 <- Fulldata %>%
  filter(Site == "Bowman") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
BO123$Sample <- 1:14

#Site and Year columns no longer necessary
BO123 <- BO123[-c(1:2)]

#Change row names to assigned sample number
BO123 <- BO123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Bowman
rarecurve(BO123)

#-------------------------------------------------------------------#
#                            Kaldenberg                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Kaldenberg samples
KA123 <- Fulldata %>%
  filter(Site == "Kaldenberg") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
KA123$Sample <- 1:14

#Site and Year columns no longer necessary
KA123 <- KA123[-c(1:2)]

#Change row names to assigned sample number
KA123 <- KA123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Kaldenberg
rarecurve(KA123)

#-------------------------------------------------------------------#
#                             McClellan                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only McClellan samples
MC123 <- Fulldata %>%
  filter(Site == "McClellan") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
MC123$Sample <- 1:14

#Site and Year columns no longer necessary
MC123 <- MC123[-c(1:2)]

#Change row names to assigned sample number
MC123 <- MC123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for McClellan
rarecurve(MC123)

#-------------------------------------------------------------------#
#                              Sloan                                #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Sloan samples
SL123 <- Fulldata %>%
  filter(Site == "Sloan") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
SL123$Sample <- 1:14

#Site and Year columns no longer necessary
SL123 <- SL123[-c(1:2)]

#Change row names to assigned sample number
SL123 <- SL123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Sloan
rarecurve(SL123)

#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Sheller samples
SH123 <- Fulldata %>%
  filter(Site == "Sheller") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
SH123$Sample <- 1:14

#Site and Year columns no longer necessary
SH123 <- SH123[-c(1:2)]

#Change row names to assigned sample number
SH123 <- SH123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Sheller
rarecurve(SH123)

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Cretsinger samples
CR123 <- Fulldata %>%
  filter(Site == "Cretsinger") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
CR123$Sample <- 1:14

#Site and Year columns no longer necessary
CR123 <- CR123[-c(1:2)]

#Change row names to assigned sample number
CR123 <- CR123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Cretsinger
rarecurve(CR123)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Peckumn samples
PE123 <- Fulldata %>%
  filter(Site == "Peckumn") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
PE123$Sample <- 1:10

#Site and Year columns no longer necessary
PE123 <- PE123[-c(1:2)]

#Change row names to assigned sample number
PE123 <- PE123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Peckumn
rarecurve(PE123)






