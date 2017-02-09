##############################################
################### BEES #####################
##############################################

#Set working directory; depends on which computer
#Personal:
setwd("~/ISU/Project/Data/Bees")
#Lab:
setwd("C:/Users/mmackert/Box Sync/Project/Data/Bees")

#Load libraries
library(ggplot2)

#Read in the data
beefamilies <- read.csv("2016 Bee IDs.csv")

#Create a table to visualize the data
table(beefamilies$Family)

#Determine proportions of each family compared to the total
proportions <- table(beefamilies$Family)/sum(table(beefamilies$Family))

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
  ggtitle("Bee Families by Site") +
  theme_bw() +
  theme(legend.key.size = unit(1, "cm")) +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(y = "Count")
