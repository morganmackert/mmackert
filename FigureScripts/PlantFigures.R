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

#Read in the data
plants <- read.csv("simpleplants.csv")

ggplot(plants, aes(x = Date, y = TotalPlants)) +
  geom_line(aes(color = Site))










