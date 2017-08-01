##############################################
################### BEES #####################
##############################################

#Clear environment and set working directory
#Personal:
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
library(ggplot2)

#Read in the data
beefamilies <- read.csv("bees/working/BeeIDs2016.csv")
simplebees <- read.csv("R Class/bees/working/simplebees.csv")
simpleplants <- read.csv("R Class/plants/working/simpleplants.csv")

#Data file is imported with a bunch of empty columns. Cut them off.
beefamilies <- beefamilies[,-(10:29)]

#Create a table to visualize the data
table(beefamilies$Family)

#Andrendidae -> Andrenidae
#This step not necessary any longer; fixed the typo in the original datasheet
beefamilies$Family[beefamilies$Family == "Andrendidae"] <- "Andrenidae"

#Determine proportions of each family compared to the total
proportions <- table(beefamilies$Family)/sum(table(beefamilies$Family))
proportions

#Create bar graph of family count data
ggplot(beefamilies, aes(x = Family)) + 
  geom_bar()

#Plot the count data by site using facet wrap
ggplot(beefamilies, aes(x = Family)) + 
  geom_bar() + 
  facet_wrap( ~ Site)

#Create bar graph of family count data with bars broken up by site
#This one is the best
#OMG SO COOL
ggplot(beefamilies, aes(x = Family, fill = Site)) + 
  geom_bar(color = "black") +
  ggtitle("Abundance of Bee Families by Site") +
  theme_bw() +
  theme(legend.key.size = unit(1, "cm")) +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme(axis.title = element_text(size = 16, face = "bold")) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(size = 12, margin = margin(0, 0, 0, 10))) +
  labs(y = "Count")

#########################################################################################
#Total bees by date (point)
beesbysite <- ggplot(simplebees, aes(x = Date, y = TotalBees)) + 
  geom_point(shape = 19, size = 2) + 
  geom_smooth(method = lm) +
  ggtitle("Bee Abundance by Date") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme_bw() +
  labs(x = "Date", y = "Number of Individuals")
beesbysite

#Total plants by date (point)
plantsbysite <- ggplot(simpleplants, aes(x = Date, y = TotalPlants)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = lm)+
  ggtitle("Blooming Forb Richness by Date") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme_bw() +
  labs(x = "Date", y = "Number of Species")
plantsbysite

#Total bees by site (line) and by date
beesbysitedate <- ggplot(summarybees, aes(x = Date, y = total.bees)) +
  geom_line(aes(color = Site)) +
  ggtitle("Bee Abundance by Date and Site") +
  theme_bw() +
  labs(y = "Count")
beesbysitedate

#Blooming forb richness by site (line) and by date
plantsbysitedate <- ggplot(plants, aes(x = Date, y = TotalPlants)) +
  geom_line(aes(color = Site)) +
  ggtitle("Blooming Forb Richness by Site and Date") +
  theme_bw() +
  labs(y = "Number of Species")
plantsbysitedate

##############################################
#                    PLANTS                  #
##############################################

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

