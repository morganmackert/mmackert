#-------------------------------------------------------------------#
#                     Spearman Rank Correlation                     #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(lubridate)
library(dplyr)
library(tibble)
library(PerformanceAnalytics)

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

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Use lubridate to allow R to recognize the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Only dealing with 2014-2016, so remove 2017
Data123 <- Fulldata %>%
  filter(Year <= "3")
  
#Determine average floral coverage for each site during each year
floralcover123 <- Data123 %>%
  select(Year, Site, Floral.Cover) %>%
  group_by(Year, Site) %>%
  summarise(Average.FloralCover = mean(Floral.Cover))

#Determine number of blooming species found in quadrats at each site during each year
bsquadrats123 <- Data123 %>%
  select(Year, Site, Blooming.Species) %>%
  group_by(Year, Site) %>%
  summarise(Average.BSQuadrats = mean(Blooming.Species))

#Determine average bare ground coverage for each site during each year
bareground123 <- Data123 %>%
  select(Year, Site, Bare.Ground) %>%
  group_by(Year, Site) %>%
  summarise(Average.BareGround = mean(Bare.Ground))

#Join datasets together (takes two steps to join all three, dumb)
Spearman123 <- full_join(floralcover123, bsquadrats123, by = c("Year", "Site"))
Spearman123 <- full_join(Spearman123, bareground123, by = c("Year", "Site"))

#Now we can remove Year and Site columns
Spearman123 <- Spearman123[-c(1:2)]

#Perform Spearman's Rank Correlation test
Spearman123cor <- cor(Spearman123, method = "spearman")

#Export output as .csv file
write.csv(Spearman123cor, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SpearmanRank/SpearmanRank123.csv")

#Here's a weird chart too
chart.Correlation(Spearman123, method = "spearman")