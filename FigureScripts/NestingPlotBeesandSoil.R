#-------------------------------------------------------------------#
#                      Bees ~ Soil composition                      #
#-------------------------------------------------------------------#

#Research Question:  Does soil composition influence bee abundance/richness within the nesting plots?

#Objectives:  
#Visualize soil composition via ternary graphs (a la Cane 1991) of each site and all of the sites together
#Compare soil composition to bee abundance and bee richness; significant differences?

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(ggplot2)
library(ggtern)
library(dplyr)

#Read in data
Soils <- read.csv("Soil/Reduced Analysis Results.csv")
Bees <- read.csv("Bees/Bee IDs.csv")

#Rename columns in "Reduced Analysis Results"
colnames(Soils) <- c("Sample ID", "Site", "Depth", "Sand", "Silt", "Clay", "Texture")

#Subset Bees to include only nesting plot bees and remove wasps and unidentifiable specimens
BeesNP <- Bees %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site)) %>%
  filter(Trap == "Plot")

#Determine number of bee species collected by site
BeesNPbysite <- BeesNP %>%
  group_by(Site) %>%
  count(Binomial)

#Create full ternary plot
#Set "sitecolors" to be "Set1" from color brewer; makes formatting the legend easier
sitecolors <- RColorBrewer::brewer.pal(8, "Set1")

#Create full ternary diagram colored by site
fullsoilstern <- ggtern(data = Soils,
                        aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(color = Site),
             size = 4) +
  geom_point(shape = 21,
             size = 4,
             color = "black") +
  ggtitle("Soil Composition within Nesting Plots \nof All Sites") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = sitecolors,
                                                  color = "black"))) +
  scale_color_manual(values = sitecolors)
fullsoilstern

#Map bee species collected at each site on soil characteristics ternary graph

