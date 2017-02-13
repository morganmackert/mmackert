##############################################
################## PLANTS ####################
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









