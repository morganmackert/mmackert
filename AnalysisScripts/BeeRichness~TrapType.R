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
library(goeveg)

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

#Create new column in BeeIDs for the year
BeeIDs$Year <- year(BeeIDs$Date)

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

#Convert from long to wide format
BeeIDs123TTcountwide <- spread(BeeIDs123TTcount, Trap, n)

#Export to .csv
#write.csv(BeeIDs123TTcountwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness~TrapType/BeeSpeciesbyTrapType123.csv")

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
Bee.Community <- BeeIDs123TTcountsitewide[, -which(names(BeeIDs123TTcountsitewide) %in% c("Trap", "Site"))]

#Use metaMDS in vegan to create a "dissimilarity matrix" to measure similarity between samples; use k = 2 to denote the number of dimensions we're reducing to
Bee.Community.mds <- metaMDS(comm = Bee.Community,
                             autotransform = FALSE,
                             k = 2,
                             trymax = 500)

#Check to make sure we're using the correct number of dimensions; 2 is good
dimcheckMDS(Bee.Community)

#Check stress value; if over 0.2 will have to figure out something else
#Stress value provides a measure of the degree to which the distance between samples in reduced dimensional space corresponds to the actual multivariate distance between the samples. Lower stress values indicate greater conformity.
Bee.Community.mds$stress
#We're good!

#Plot it
ordiplot(Bee.Community.mds)
ordihull(Bee.Community.mds,
         groups = BeeIDs123TTcountsitewide$Trap,
         label = TRUE)

#Communicating results: The written description of the MDS analyses (often in the figure legend) should mention what dissimilarity metric was used, whether data were transformed or standardised, and present the stress value. In a formal analysis, MDS plots are usually accompanied by some multivariate statistical test of dissimilarity between treatment/observational groups, e.g., via the adonis function in vegan
#http://environmentalcomputing.net/multidimensional-scaling/

#Try K-means stuff?


#-------------------------------------------------------------------#
#          Bee Species Richness by Sample Period ~ Trap Type        #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Research Question: How does bee species richness vary with respect of the type of trap used to collect them? Does this number also vary with sampling period? Can we maximize our collection effort by using fewer, more effective trapping methods?

#Objectives:
#Create model(s) to explore relationship between bee species richness and trap type
#Use created model(s) to visualize the relationship graphically

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Group BeeIDs by Trap Type
BeeIDs1234TTcount <- BeeIDs1234 %>%
  group_by(Trap) %>%
  count(Binomial)

#Convert from long to wide format
BeeIDs1234TTcountwide <- spread(BeeIDs1234TTcount, Trap, n)

#Export to .csv
#write.csv(BeeIDs1234TTcountwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness~TrapType/BeeSpeciesbyTrapType1234.csv")

#Group BeeIDs by Site and Trap Type
BeeIDs1234TTcountsite <- BeeIDs1234 %>%
  group_by(Trap, Site) %>%
  count(Binomial)

#Determine number of species collected by each trap type
BeeIDs1234TTspp <- BeeIDs1234 %>%
  group_by(Trap) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Plot: Bee Species Richness vs. Trap Type plot using ggplot2
BSbyTT1234plot <- ggplot(BeeIDs1234TTspp, aes(x = Trap,
                                              y = Total.Species)) +
  geom_bar(stat = "identity",
           color = "black") +
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
BSbyTT1234plot

#Group BeeIDs123 by site and date
BeeIDs1234TTsitedate <- BeeIDs1234 %>%
  group_by(Site, Date, Trap) %>%
  summarise(Total.Species = length(unique(Binomial)))

#Test for significance between groups
#Run an ANOVA to test for significance of bee species richness based on trap type
BSbyTT1234ANOVA <- aov(Total.Species ~ Trap,
                       data = BeeIDs1234TTsitedate)
summary(BSbyTT1234ANOVA)

#Based on the outcome of the ANOVA (p < 0.001), we know that there are significant differences between the number of bee species collected by each trap type.

#Use Tukey Honest Significant Differences function to perform multiple pairwise-comparisons between the means of each trap type.
TukeyHSD(BSbyTT1234ANOVA)

#Reformat BeeIDs123TTcountsite from long to wide
BeeIDs1234TTcountsitewide <- spread(BeeIDs1234TTcountsite, Binomial, n)

#Fill NAs with 0
BeeIDs1234TTcountsitewide[is.na(BeeIDs1234TTcountsitewide)] <- 0

#Make another data frame removing the Trap and Site columns from BeeIDs123TTcountsitewide
Bee.Community1234 <- BeeIDs1234TTcountsitewide[, -which(names(BeeIDs1234TTcountsitewide) %in% c("Trap", "Site"))]

#Use metaMDS in vegan to create a "dissimilarity matrix" to measure similarity between samples; use k = 2 to denote the number of dimensions we're reducing to
Bee.Community1234.mds <- metaMDS(comm = Bee.Community1234,
                                 autotransform = FALSE,
                                 k = 2,
                                 trymax = 500)

#Check to make sure we're using the correct number of dimensions; 2 is good
dimcheckMDS(Bee.Community1234)

#Check stress value; if over 0.2 will have to figure out something else
#Stress value provides a measure of the degree to which the distance between samples in reduced dimensional space corresponds to the actual multivariate distance between the samples. Lower stress values indicate greater conformity.
Bee.Community1234.mds$stress
#We're good!

#Plot it
ordiplot(Bee.Community1234.mds)
ordihull(Bee.Community1234.mds,
         groups = BeeIDs1234TTcountsitewide$Trap,
         label = TRUE)