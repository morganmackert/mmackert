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
BA<-read.csv("Bee_BoxPlot_Abundance.csv")

#Reorder PlantDiversity categories
BA$PlantDiversity <- factor(BA$PlantDiversity, c("LowDiversity", "MediumDiversity", "HighDiversity"))

#Reorder Period categories
BA$Period <- factor(BA$Period,c("EarlyMay","LateMay","June","July","August"))

#the contrast letters go medium, high, low instead of low, medium, high
#####If the letters are in the wrong order, is there a way to fix it?
#####How did you determine these letters?
#####Why do you have to make the second dataframe?
contrast<-c("a","b","a","b","b","a","ab","b","a","ab","b","a","b","b","a")
contrast.df <- data.frame(contrast=contrast)
contrast.df$PlantDiversity <- rep(c("LowDiversity", "MediumDiversity", "HighDiversity"), 5)
contrast.df$Period<-rep(c("EarlyMay","LateMay","June","July","August"), each=3)
contrast.df$Abundance <- 1
#####Abundance = 1? Why?

#Plot Sampling Period vs. Bee Species Richness with Plant Diversity as fill
ggplot(data = BA, aes(x = Period, y = Abundance, fill = PlantDiversity)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  labs(x = "Sampling Period", y = "Bee Abundance") +
  theme_bw() +
  ggtitle("Bee Abundance as a Result of \nSampling Period and Plant Diversity") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  geom_text(data = contrast.df, aes(label = contrast), position = position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black"))

#-------------------------------------------------------------------#
#                    Bee Species Richness Boxplot                   #
#-------------------------------------------------------------------#

#Read in data
BSR<-read.csv("Bee_Richness_Time_Curve.csv")

#Reorder PlantDiversity categories
BSR$PlantDiversity <- factor(BSR$PlantDiversity, c("LowDiversity", "MediumDiversity", "HighDiversity"))

#Reorder Period categories
BSR$Period<-factor(BSR$Period,c("EarlyMay","LateMay","June","July","August"))

#the contrast letters go medium, high, low instead of low, medium, high
contrast <- c("a","b","a","b","b","a","a","b","a","a","a","a","a","a","a")
contrast.df <- data.frame(contrast = contrast)
contrast.df$PlantDiversity <- rep(c("LowDiversity", "MediumDiversity", "HighDiversity"), 5)
contrast.df$Period <- rep(c("EarlyMay","LateMay","June","July","August"), each = 3)
contrast.df$Species <- 1

#Plot Sampling Period vs. Bee Species Richness with Plant Diversity as fill
ggplot(data = BSR, aes(x = Period, y = Species, fill = PlantDiversity)) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  labs(x = "Sampling Period", y = "Bee Species Richness") +
  theme_bw() +
  ggtitle("Bee Species Richness as a Result of \nSampling Period and Plant Diversity") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  geom_text(data = contrast.df, aes(label = contrast), position = position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black"))