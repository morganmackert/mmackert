#-------------------------------------------------------------------#
#                   Sampling Effort ~ Bee Species                   #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How many sampling events are required to reach bee species saturation?

#Objectives:
#Create model(s) to explore relationship between sampling effort and the number of bee species collected as a result
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(iNEXT)
library(ggplot2)

#Read in data
iNEXTAbundance <- read.csv("mmackert/Data/bees/working/iNEXTAbundance.csv")

#Data must be as a dataframe if not brought in as dataframe.
iNEXTAbundance <- as.data.frame(iNEXTAbundance)

#Change the rownames to the species rather than numbers
iNEXTAbundance2 <- iNEXTAbundance[,-1]
rownames(iNEXTAbundance2) <- iNEXTAbundance[,1]

#Check the dataframe to make sure all is well
str(iNEXTAbundance2)

#RUN THE TEST
iNEXTResult <- iNEXT(iNEXTAbundance2, q = c(0, 1, 2), datatype = "abundance")
#q = Hill Number; 0 = species richness, 1 = Shannon Diversity, 2 = Simpson Diversity

#Make pretty graphs
ggiNEXT(iNEXTResult, type = 1, facet.var = "site")

