###############################################################
################   BEES VS. PLANTS ANALYSIS  ##################
###############################################################


#Research question: How do floral resources influence bee abundance?

#Load libraries
library(ggplot2)
library(lubridate)
library(lme4)
library(tidyr)
library(lsmeans)

#View data
summary(bees)

#Plot data to look for outliers
hist(beesbysite$Num)
boxplot(beesbysite$Num ~ beesbysite$Site, xlab = "Site", ylab = "Number of Individuals")

#Basic general linear model
beesvsplants <- glm(formula = bind$TotalBees ~ bind$TotalPlants, family = poisson)
summary(beesvsplants)
plot(beesvsplants)

#General linear model including "Site"
beesvsplantsbysite <- glm(formula = bind$TotalBees ~ bind$Site + bind$TotalPlants, family = poisson)
summary(beesvsplantsbysite)
plot(beesvsplantsbysite)

#General linear model including "Site" and "Date"
#This is an interaction! Must do "lsmeans"
#See lsmeans blurb below
beesvsplantsbysitedate <- glm(TotalBees ~ Date * TotalPlants + Site, family = poisson, data=bind)
summary(beesvsplantsbysitedate)
 plot(beesvsplantsbysitedate)

#Make a grid for reasons
TotalBeesgrid <- ref.grid(beesvsplantsbysitedate)

#Specify the grid and the desired means
#Get estimates with SE and CIs
#"Least Squares Means can be defined as a linear combination (sum) of the estimated effects (means, etc) from a linear model. These means are based on the model used."

Datelsmeans <- lsmeans(TotalBeesgrid, "Date")
TotalPlantslsmeans <- lsmeans(TotalBeesgrid, "TotalPlants")

summary(Datelsmeans)
summary(TotalPlantslsmeans)
plot(Datelsmeans)

#Try percent cover and number of species for plants
#But how?