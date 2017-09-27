#-------------------------------------------------------------------#
#                  Bee Species Richness ~ Trap Type                 #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Research Question: How does bee species richness vary with respect of the type of trap used to collect them? Can we maximize our collection effort by using fewer, more effective traps?

#Objectives:
#Create model(s) to explore relationship between bee species richness and trap type
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(ggplot2)

#Read in data
BSTT1234 <- read.csv("Bees/Species Richness by Trap extended.csv")
#Site = Site name
#Trap names correspond to the number of bee species collected with that trap type throughout years 1-4.

#Set "Site" as a factor
BSTT1234$Site <- as.factor(BSTT1234$Site)

#Plot: Bee Species Richness vs. Trap Type plot using ggplot2
BSbyTT1234plot <- ggplot(BSTT1234, aes(x = Site, y = Species.Richness)) +
  geom_bar(aes(fill = Trap), position = "dodge", stat = "identity", color = "black") +
  scale_fill_manual(labels = c("Bee bowls", "Blue vane", "Emergence", "Non-Target", "Pitfall", "Plots"), values = c("deepskyblue4", "darkorchid1", "#FFB90F", "#000000", "darkgreen", "#D55E00")) +
  theme_bw() +
  labs(x = "Trap Type", y = "Number of Bee Species") +
  ggtitle("Number of Bee Species Collected by \nDifferent Trap Types") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
BSbyTT1234plot

#Test for significance between groups
#Run an ANOVA to test for significance of bee species richness based on trap type
BSbyTT1234ANOVA <- aov(Species.Richness ~ Trap, data = BSTT1234)
summary(BSbyTT1234ANOVA)

#Based on the outcome of the ANOVA (p < 0.001), we know that there are significant differences between the number of bee species collected by each trap type.

#Use Tukey Honest Significant Differences function to perform multiple pairwise-comparisons between the means of each trap type.
TukeyHSD(BSbyTT1234ANOVA)