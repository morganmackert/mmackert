## AES Consulting
## Client: Morgan Mackert <mmackert@iastate.edu>
## PI: Mary Harris <maharris@iastate.edu>
## Department: EEOB
## Consultant: Katherine Goode <kgoode@iastate.edu>
## Purpose: Code for visualzing the 3 years of plant coverage data
## Date created: 2017/09/27
## Last modified: 2017/10/3

# Load libraries
library(ggplot2)
library(dplyr)
library(plotly)

# Set working directory
setwd("~/Desktop/Consulting/Clients/Mackert, Morgan")

# Load in dataset
plant_coverage_3year <- read.csv("Data/PlantCoverage_3year.csv")


##################################################################
## Plots for looking at the distributions of total bees by year ##
##################################################################

# Historgrams of total bees separated by year:
#   We can see that all distributions are skewed to the left, but 2016 has 
#   two extreme outliers compared to other years.
ggplot(plant_coverage_3year, aes(x = Total_Bees)) +
  geom_histogram(bins = 25) + 
  facet_grid( ~ Year)

##############################################################
## Plots for looking at variability between years and sites ##
##############################################################

# Boxplot of total bees versus sampling days separated by year: 
#   This plot lets you how the total bees change between periods over the three years
ggplot(plant_coverage_3year, aes(y = Total_Bees, x = Sampling_Day, group = Sampling_Day)) +
  geom_boxplot() + 
  facet_grid( ~ Year)

# Scatterplot of total bees vs sampling day separated by site and year:
#   For visualizing the variation between years and sites
ggplot(plant_coverage_3year, aes(x = Sampling_Day, y = Total_Bees)) +
  geom_point() + 
  facet_grid(Year ~ Site)

############################################################
## Plots for comparing sampling days and day of they year ##
############################################################

# Scatterplot of total bees versus day of year separated by year and colored by sampling day:
#   For checking how sampling day relates to day of the year - I think there are some points
#   that have been given the incorrect sampling day label
ggplotly(ggplot(plant_coverage_3year, aes(x = DayofYear, y = Total_Bees)) + 
           geom_point(aes(color = factor(Sampling_Day_wrong))) + 
           facet_grid(Year ~ .))

# Scatterplot of total bees versus day of year separated by year and colored by corrected 
# sampling day:
#   I made a guess at which sampling days the dates should be edited to, but you sould
#   probably look into this.
ggplotly(ggplot(plant_coverage_3year, aes(x = DayofYear, y = Total_Bees)) + 
  geom_point(aes(color = factor(Sampling_Day))) + 
  facet_grid(Year ~ .))

################################################################################
## Plots for Visualizing Relationship between Total Bees and Average Coverage ##
################################################################################

# Scatterplot of total bees versus average coverage with a linear model fit to the data
ggplot(plant_coverage_3year, aes(x = Average_Coverage, y = Total_Bees)) + 
  geom_point() + 
  #facet_grid(Sampling_Period ~ Year) + 
  geom_smooth(method = "lm")

# Scatterplot of total bees versus average coverage with outliers removed with a linear model 
# fit to the data:
#   This plot shows how much the outliers affect the slope of the line. With the outliers 
#   excluded, there does not appear to be a strong linear relationship between total bees 
#   and average coverage like there does when the outliers are included.
ggplot(subset(plant_coverage_3year, plant_coverage_3year$Total_Bees < 200), 
       aes(x = Average_Coverage, y = Total_Bees)) + 
  geom_point() + 
  #facet_grid(Sampling_Period ~ Year) + 
  geom_smooth(method = "lm")

# Scatterplot of total bees versus average coverage with a linear model fit to the data 
# with a square root transformation applied to total bees
ggplot(plant_coverage_3year, aes(x = Average_Coverage, y = sqrt(Total_Bees))) + 
  geom_point() + 
  facet_grid(Sampling_Period ~ .) + 
  geom_smooth(method = "lm")

# Scatterplot of total bees versus average coverage with outliers removed with a linear model 
# fit to the data with a square root transformation applied to total bees:
ggplot(subset(plant_coverage_3year, plant_coverage_3year$Total_Bees < 200), aes(x = Average_Coverage, y = sqrt(Total_Bees))) + 
  geom_point() + 
  facet_grid(Sampling_Period ~ .) + 
  geom_smooth(method = "lm")

