##############################################
############### BEES BY FAMILY ###############
##############################################

#Clear environment
rm(list=ls())

#Load libraries
library(ggplot2)

#Read in the data
beefamilies <- read.csv("https://raw.githubusercontent.com/morganmackert/mmackert/master/Data/bees/working/2016%20Bee%20IDs.csv")

#Create a table to visualize the data
table(beefamilies$Family)

#Determine proportions of each family compared to the total
proportions <- table(beefamilies$Family)/sum(table(beefamilies$Family))

#Create bar graph of family count data
ggplot(beefamilies, aes(x = Family)) + 
  geom_bar() +
  labs(y = "Count") +
  theme_bw()

#Plot the count data by site using facet wrap
##### MORGAN: How to angle the labels??
ggplot(beefamilies, aes(x = Family)) + 
  geom_bar() + 
  labs(y = "Count") +
  facet_wrap( ~ Site) +
  theme_bw()

#Create bar graph of family count data with bars broken up by site
#This one is the best
#SO COOL
ggplot(beefamilies, aes(x = Family, fill = Site)) + 
  geom_bar(color = "black") +
  ggtitle("Abundance of Bee Families by Site") +
  theme_bw() +
  theme(legend.key.size = unit(1, "cm")) +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme(axis.title = element_text(size = 16, face = "bold")) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12, margin = margin(0, 0, 0, 10))) +
  labs(y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------#
#                Extras - Bees
#------------------------------------------------#
#Set working directory; depends on which computer
#Personal:
setwd("~/ISU/Project/mmackert/Data/bees/working")
#Lab:
setwd("C:/Users/mmackert/Box Sync/Project/Data/Plants")

#Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

#Read in the data
simplebees <- read.csv("simplebees.csv")

#Total bees by date (point) with subsetted data
beesbysite <- ggplot(simplebees, aes(x = Date, y = TotalBees)) + 
  geom_point(shape = 19, size = 2) + 
  geom_smooth(method = lm) +
  ggtitle("Bee Abundance by Date") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme_bw() +
  labs(x = "Date", y = "Number of Individuals")
beesbysite

#Total bees by site (line) and by date with subsetted data
beesbysitedate <- ggplot(simplebees, aes(x = Date, y = TotalBees)) +
  geom_line(aes(color = Site)) +
  ggtitle("Bee Abundance by Date and Site") +
  theme_bw() +
  labs(y = "Count")
beesbysitedate

#------------------------------------------------#
#                Extras - Plants
#------------------------------------------------#

#Set working directory; depends on which computer
#Personal:
setwd("~/ISU/Project/mmackert/Data/plants/working")
#Lab:
setwd("C:/Users/mmackert/Box Sync/Project/Data/Plants")

#Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

#Read in the data
plants <- read.csv("simpleplants.csv")

#Fix dates
plants$Date <- mdy(plants$Date)

#Plot total number of blooming forb species as points organized by date
plantsbydate <- ggplot(plants, aes(x = Date, y = TotalPlants)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = lm)+
  ggtitle("Blooming Forb Richness by Date") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Species")
plantsbydate

#Plot total number of blooming forb species as lines (sites) organized by date
plantsbysitedate <- ggplot(plants, aes(x = Date, y = TotalPlants)) +
  geom_line(aes(color = Site)) +
  ggtitle("Blooming Forb Richness by Site and Date") +
  theme_bw() +
  labs(y = "Number of Species")
plantsbysitedate

#Total plants by date (point) with subsetted data
plantsbysite <- ggplot(plants, aes(x = Date, y = TotalPlants)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = lm)+
  ggtitle("Blooming Forb Richness by Date") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme_bw() +
  labs(x = "Date", y = "Number of Species")
plantsbysite

#------------------------------------------------#
#                Extras - Code
#------------------------------------------------#

Andrendidae -> Andrenidae
##### MORGAN: Still leaves the column heading. Figure out how to remove.
##### MORGAN: Fix the two blanks.
beefamilies$Family[beefamilies$Family == "Andrendidae"] <- "Andrenidae"















