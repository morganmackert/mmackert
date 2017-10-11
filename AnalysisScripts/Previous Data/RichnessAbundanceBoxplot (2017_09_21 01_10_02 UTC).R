#####################################################################
#                              BOXPLOTS                             #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Previous Data/Data Files")

#Load libraries
library(ggplot2)
library(plyr)

#-------------------------------------------------------------------#
#                        Bee Abundance Boxplot                      #
#-------------------------------------------------------------------#

#Read in data
BA <- read.csv("Bee_BoxPlot_Abundance.csv")
#Abundance in this dataframe refers to bee abundance

#Set PlantDiversity column as a factor
BA$PlantDiversity <- factor(BA$PlantDiversity, c("LowDiversity", "MediumDiversity", "HighDiversity"))

#Set Period column as a factor
BA$Period <- factor(BA$Period, c("EarlyMay", "LateMay", "June", "July", "August"))

#The contrast letters go medium, high, low instead of low, medium, high
#####If the letters are in the wrong order, is there a way to fix it? Gotta be a better way than this.
#####How did you determine these letters?
#####In SAS.
#####Why do you have to make the second dataframe?
#####Formatting contrast letters.

#Create group of contrast letters
BAcontrast <- c("a", "b", "a", "b", "b", "a", "ab", "b", "a", "ab", "b", "a", "b", "b", "a")

#Create dataframe with contrast letters
BAcontrast.df <- data.frame(BAcontrast = BAcontrast)

#Add "PlantDiversity" column into contrast dataframe with five repetitions of the categorical plant diversity levels, cycling through each level
BAcontrast.df$PlantDiversity <- rep(c("LowDiversity", "MediumDiversity", "HighDiversity"), 5)

#Add "Period" column into contrast dataframe with three repetitions of the sampling periods, keeping them in groups
BAcontrast.df$Period <- rep(c("EarlyMay", "LateMay", "June", "July", "August"), each = 3)

#Add "Abundance" column into contrast dataframe with only values of "1" to keep the letters at the bottom of the graph
BAcontrast.df$Abundance <- 1
#####Abundance = 1? Why?
#####Keeps letters at the bottom of the graph

#Plot Sampling Period vs. Bee Species Richness with Plant Diversity as fill
BABoxplot <- ggplot(data = BA, aes(x = Period, y = Abundance, fill = PlantDiversity)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  labs(x = "Sampling Period", y = "Bee Abundance") +
  theme_bw() +
  ggtitle("Bee Abundance as a Result of \nSampling Period and Plant Diversity") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  geom_text(data = BAcontrast.df, aes(label = BAcontrast), position = position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("EarlyMay" = "Early May", "LateMay" = "Late May")) +
  scale_fill_discrete("Plant Diversity", labels = c("LowDiversity" = "Low Diversity", "MediumDiversity" = "Medium Diversity", "HighDiversity" = "High Diversity"))
BABoxplot

#-------------------------------------------------------------------#
#                    Bee Species Richness Boxplot                   #
#-------------------------------------------------------------------#

#Read in data
BSR<-read.csv("Bee_Richness_Time_Curve.csv")

#Set PlantDiversity column as a factor
BSR$PlantDiversity <- factor(BSR$PlantDiversity, c("LowDiversity", "MediumDiversity", "HighDiversity"))

#Set Period column as a factor
BSR$Period <- factor(BSR$Period, c("EarlyMay", "LateMay", "June", "July",  "August"))

#Create dataframe with contrast letters
BSRcontrast <- c("a", "b", "a", "b", "b", "a", "a", "b", "a", "a", "a", "a", "a", "a", "a")

#Create dataframe with contrast letters
BSRcontrast.df <- data.frame(BSRcontrast = BSRcontrast)

#Add "PlantDiversity" column into contrast dataframe with five repetitions of the categorical plant diversity levels, cycling through each level
BSRcontrast.df$PlantDiversity <- rep(c("LowDiversity", "MediumDiversity", "HighDiversity"), 5)

#Add "Period" column into contrast dataframe with three repetitions of the sampling periods, keeping them in groups
BSRcontrast.df$Period <- rep(c("EarlyMay", "LateMay", "June", "July", "August"), each = 3)

#Add "Abundance" column into contrast dataframe with only values of "1" to keep the letters at the bottom of the graph
BSRcontrast.df$Species <- 1

#Plot Sampling Period vs. Bee Species Richness with Plant Diversity as fill
BSRBoxplot <- ggplot(data = BSR, aes(x = Period, y = Species, fill = PlantDiversity)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  labs(x = "Sampling Period", y = "Bee Species Richness") +
  theme_bw() +
  ggtitle("Bee Species Richness as a Result of \nSampling Period and Plant Diversity") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  geom_text(data = BSRcontrast.df, aes(label = BSRcontrast), position = position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  scale_x_discrete(labels=c("EarlyMay" = "Early May", "LateMay" = "Late May")) +
  scale_fill_discrete("Plant Diversity", labels = c("LowDiversity" = "Low Diversity", "MediumDiversity" = "Medium Diversity", "HighDiversity" = "High Diversity"))
BSRBoxplot
