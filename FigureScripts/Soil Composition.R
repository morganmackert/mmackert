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
  geom_point(shape = 21, size = 3, fill = "#FFFF33") +
  ggtitle("Plunkett Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
PLsoilstern

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Create Bowman dataframe
BOsoils <- soils[33:40, ]

#Create Bowman ternary diagram 
BOsoilstern <- ggtern(data = BOsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#E41A1C") +
  ggtitle("Bowman Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
BOsoilstern

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Create Kaldenberg dataframe
KAsoils <- soils[1:8, ]

#Create Kaldenberg ternary diagram 
KAsoilstern <- ggtern(data = KAsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#4DAF4A") +
  ggtitle("Kaldenberg Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
KAsoilstern

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Create McClellan dataframe
MCsoils <- soils[17:24, ]

#Create McClellan ternary diagram 
MCsoilstern <- ggtern(data = MCsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#984EA3") +
  ggtitle("McClellan Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
MCsoilstern

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Create Sloan dataframe
SLsoils <- soils[57:64, ]

#Create Sloan ternary diagram 
SLsoilstern <- ggtern(data = SLsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#F781BF") +
  ggtitle("Sloan Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
SLsoilstern

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Create Sheller dataframe
SHsoils <- soils[41:48, ]

#Create Sheller ternary diagram 
SHsoilstern <- ggtern(data = SHsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#A65628") +
  ggtitle("Sheller Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
SHsoilstern

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Create Cretsinger dataframe
CRsoils <- soils[9:16, ]

#Create Cretsinger ternary diagram 
CRsoilstern <- ggtern(data = CRsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#377EB8") +
  ggtitle("Cretsinger Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
CRsoilstern

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Create Peckumn dataframe
PEsoils <- soils[25:32, ]

#Create Peckumn ternary diagram 
PEsoilstern <- ggtern(data = PEsoils, aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#FF7F00") +
  ggtitle("Peckumn Soil Composition") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15)) +
  theme_showarrows()
PEsoilstern

#-------------------------------------------------------------------#
#                                Full                               #
#-------------------------------------------------------------------#
#Set "sitecolors" to be "Set1" from color brewer; makes formatting the legend easier
sitecolors <- RColorBrewer::brewer.pal(8, "Set1")

#Create full ternary diagram colored by site
fullsoilstern <- ggtern(data = soils, aes(Sand, Silt, Clay)) +
  geom_point(aes(color = Site), size = 3) +
  geom_point(shape = 21, size = 3, color = "black") +
  ggtitle("Soil Composition of All Sites") +
  theme_bw() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.75)) +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21, fill = sitecolors, color = "black"))) +
  scale_color_manual(values = sitecolors)
fullsoilstern