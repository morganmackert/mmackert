#####################################################################
#                         SOIL COMPOSITION                          #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
library(ggplot2)
library(ggtern)

#Read in data
soils <- read.csv("soil/Reduced Analysis Results.csv")

#Rename columns in "Reduced Analysis Results"
colnames(soils) <- c("Sample ID", "Site", "Beginning Depth", "Ending Depth", "Sand", "Silt", "Clay", "Texture")

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Create Plunkett dataframe
PLsoils <- soils[49:56, ]

#Create Plunkett ternary diagram 
PLsoilstern <- ggtern(data = PLsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Plunkett Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
PLsoilstern

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Create Bowman dataframe
BOsoils <- soils[33:40, ]

#Create Bowman ternary diagram 
BOsoilstern <- ggtern(data = BOsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Bowman Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
BOsoilstern

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Create Kaldenberg dataframe
KAsoils <- soils[1:8, ]

#Create Kaldenberg ternary diagram 
KAsoilstern <- ggtern(data = KAsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Kaldenberg Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
KAsoilstern

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Create McClellan dataframe
MCsoils <- soils[17:24, ]

#Create McClellan ternary diagram 
MCsoilstern <- ggtern(data = MCsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("McClellan Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
MCsoilstern

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Create Sloan dataframe
SLsoils <- soils[57:64, ]

#Create Sloan ternary diagram 
SLsoilstern <- ggtern(data = SLsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Sloan Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
SLsoilstern

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Create Sheller dataframe
SHsoils <- soils[41:48, ]

#Create Sheller ternary diagram 
SHsoilstern <- ggtern(data = SHsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Sheller Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
SHsoilstern

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Create Cretsinger dataframe
CRsoils <- soils[9:16, ]

#Create Cretsinger ternary diagram 
CRsoilstern <- ggtern(data = CRsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Cretsinger Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
CRsoilstern

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Create Peckumn dataframe
PEsoils <- soils[25:32, ]

#Create Peckumn ternary diagram 
PEsoilstern <- ggtern(data = PEsoils, aes(Sand, Silt, Clay)) +
  geom_point() +
  ggtitle("Peckumn Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))

#-------------------------------------------------------------------#
#                                Full                               #
#-------------------------------------------------------------------#

#Define specific colors for each site
#This hasn't worked yet...
sitecolors <- c("Plunkett" = "red", "Bowman" = "orange", "Kaldenberg" = "yellow", "McClellan" = "green", "Sloan" = "blue", "Sheller" = "white", "Cretsinger" = "violet", "Peckumn" = "black")

#Create full ternary diagram colored by site
fullsoilstern <- ggtern(data = soils, aes(Sand, Silt, Clay)) +
  geom_point(aes(fill = Site, color = "sitecolors"), stat = "identity", color = "black") +
  theme_bw() +
  scale_color_manual(values = sitecolors) +
  ggtitle("Full Soil Composition") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5))
fullsoilstern