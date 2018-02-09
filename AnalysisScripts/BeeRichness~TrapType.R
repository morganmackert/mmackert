#-------------------------------------------------------------------#
#                  Bee Species Richness ~ Trap Type                 #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Research Question: How does bee species richness vary with respect to the type of trap used to collect them? Can we maximize our collection effort by using fewer, more effective traps?

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
library(tidyr)
library(vegan)

#Read in data
BeeIDs <- read.csv("Bees/Bee IDs.csv")
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)

#Fix trap names
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Group BeeIDs by Trap Type
BeeIDs123TTcount <- BeeIDs123 %>%
  group_by(Trap) %>%
  count(Binomial)

#Group BeeIDs by Site and Trap Type
BeeIDs123TTcountsite <- BeeIDs123 %>%
  group_by(Trap, Site) %>%
  count(Binomial)

#Determine number of species collected by each trap type
BeeIDs123TTspp <- BeeIDs123 %>%
  group_by(Trap) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Plot: Bee Species Richness vs. Trap Type plot using ggplot2
BSbyTT123plot <- ggplot(BeeIDs123TTspp, aes(x = Trap,
                                             y = Total.Species)) +
  geom_bar(stat = "identity",
           color = "black") +
  scale_fill_manual(labels = c("Bee bowls", "Blue vane", "Emergence", "Non-Target", "Pitfall", "Plots"),
                    values = c("deepskyblue4", "darkorchid1", "#FFB90F", "#000000", "darkgreen", "#D55E00")) +
  theme_bw() +
  labs(x = "Trap Type",
       y = "Number of Bee Species") +
  ggtitle("Number of Bee Species Collected by \nDifferent Trap Types") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10,
                                   angle = 45,
                                   hjust = 1))
BSbyTT123plot

#Group BeeIDs123 by site and date
BeeIDs123TTsitedate <- BeeIDs123 %>%
  group_by(Site, Date, Trap) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Test for significance between groups
#Run an ANOVA to test for significance of bee species richness based on trap type
BSbyTT123ANOVA <- aov(Total.Species ~ Trap,
                      data = BeeIDs123TTsitedate)
summary(BSbyTT123ANOVA)

#Based on the outcome of the ANOVA (p < 0.001), we know that there are significant differences between the number of bee species collected by each trap type.

#Use Tukey Honest Significant Differences function to perform multiple pairwise-comparisons between the means of each trap type.
TukeyHSD(BSbyTT123ANOVA)

#Reformat BeeIDs123TTcountsite from long to wide
BeeIDs123TTcountsitewide <- spread(BeeIDs123TTcountsite, Binomial, n)

#Fill NAs with 0
BeeIDs123TTcountsitewide[is.na(BeeIDs123TTcountsitewide)] <- 0

#Make another data frame removing the Trap and Site columns from BeeIDs123TTcountsitewide
Bee.Community <- BeeIDs123TTcountsitewide[3:172]

#Use metaMDS in vegan to create a "dissimilarity matrix" to measure similarity between samples; use k = 2 to denote the number of dimensions we're reducing to
Bee.Community.mds <- metaMDS(comm = Bee.Community,
                             autotransform = FALSE,
                             k = 2)

#Stressplot to visualize new distances against the original distances
stressplot(Bee.Community.mds)
#Looks okay I think?

#Check stress value; if over 0.2 will have to figure out something else
#Stress value provides a measure of the degree to which the distance between samples in reduced dimensional space corresponds to the actual multivariate distance between the samples. Lower stress values indicate greater conformity.
Bee.Community.mds$stress
#We're good!

#Plot it
ordiplot(Bee.Community.mds)
ordiellipse(Bee.Community.mds,
         groups = BeeIDs123TTcountsitewide$Trap)

#Extract x and y coordinates
Bee.Community.mds.xy <- data.frame(Bee.Community.mds$points)

#Add in site and trap type factors
Bee.Community.mds.xy$Site <- BeeIDs123TTcountsitewide$Site
Bee.Community.mds.xy$Trap <- BeeIDs123TTcountsitewide$Trap

#Plot again
Bee.Community.mds.plot <- ggplot(Bee.Community.mds.xy,
                                 aes(x = MDS1,
                                     y = MDS2,
                                     color = Trap)) +
  geom_path(data = Bee.Community.mds.xy,
            aes(x = NMDS1,
                y = NMDS2)) +
  geom_point() +
  theme_bw()

##Communicating results: The written description of the MDS analyses (often in the figure legend) should mention what dissimilarity metric was used, whether data were transformed or standardised, and present the stress value. In a formal analysis, MDS plots are usually accompanied by some multivariate statistical test of dissimilarity between treatment/observational groups, e.g., via the adonis function in vegan
#http://environmentalcomputing.net/multidimensional-scaling/

#-------------------------------------------------------------------#
#          Bee Species Richness by Sample Period ~ Trap Type        #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Research Question: How does bee species richness vary with respect of the type of trap used to collect them? Does this number also vary with sampling period? Can we maximize our collection effort by using fewer, more effective trapping methods?

#Objectives:
#Create model(s) to explore relationship between bee species richness and trap type
#Use created model(s) to visualize the relationship graphically

#Read in data
BSTTSP123 <- read.csv("Bees/Bee Species by Trap by Sampling Period 123.csv")
#Trap Type = trap used to collect bees
#Months = Corresponds to the sampling period in which samples were taken
#Values = Number of bee species collected in each sample period by trap type

#Plot: Bee Species Richness vs. Trap Type plot using ggplot2
BSbyTTSP123plot <- ggplot(BSTTSP123, aes(x = Sampling.Period,
                                        y = Number.Bee.Species)) +
  geom_bar(aes(fill = Trap),
           stat = "identity",
           color = "black") +
  scale_fill_manual(labels = c("Bee bowls", "Blue vane", "Emergence", "Non-Target", "Pitfall"),
                    values = c("deepskyblue4", "darkorchid1", "#FFB90F", "#000000", "darkgreen")) +
  theme_bw() +
  labs(x = "Sampling Period",
       y = "Number of Bee Species") +
  ggtitle("Number of Bee Species Collected by \nDifferent Trap Types") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10,
                                   hjust = 0.5))
BSbyTTSP123plot

#Test for significance between groups
#Run an ANOVA to test for significance of bee species richness based on trap type
BSbyTTSP123ANOVA <- aov(Number.Bee.Species ~ Trap + Sampling.Period, data = BSTTSP123)
summary(BSbyTTSP123ANOVA)

#Based on the outcome of the ANOVA (p < 0.001), we know that there are significant differences between the number of bee species collected by each trap type.

#Use Tukey Honest Significant Differences function to perform multiple pairwise-comparisons between the means of each trap type.
TukeyHSD(BSbyTTSP123ANOVA)
