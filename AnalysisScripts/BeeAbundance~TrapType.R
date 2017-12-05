#-------------------------------------------------------------------#
#                      Bee Abundance ~ Trap Type                    #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Research Question: How does bee species richness vary with respect of the type of trap used to collect them? Can we maximize our collection effort by using fewer, more effective traps?

#Objectives:
#Create model(s) to explore relationship between bee species richness and trap type
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(ggplot2)

#Read in data
Fulldata <- read.csv("Combined full data set.csv")
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

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Use lubridate to allow R to recognize the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Only dealing with 2014-2016, so remove 2017
Data123 <- Fulldata %>%
  filter(Year <= 3)

#Year column is brought in as an integer. Change to factor for Morgan's plot.
Data123$Year <- as.factor(Data123$Year)

#Determine total number of bees collected in blue vanes by site and year
BAbyBV123 <- Data123 %>%
  group_by(Site) %>%
  summarise(Total.BlueVane = sum(Blue.Vane.Abundance))

#Determine total number of bees collected in bee bowls by site and year
BAbyBB123 <- Data123 %>%
  group_by(Site) %>%
  summarise(Total.BeeBowls = sum(Bee.Bowls.Abundance))

#Determine total number of bees collected by non-target by site and year
BAbyNT123 <- Data123 %>%
  group_by(Site) %>%
  summarise(Total.NT = sum(Non.Target.Sweeping.Abundance))

#Determine total number of bees collected in emergence traps by site and year
BAbyET123 <- Data123 %>%
  group_by(Site) %>%
  summarise(Total.Emergence = sum(Emergence.Traps.Abundance))
