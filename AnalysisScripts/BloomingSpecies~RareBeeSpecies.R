#-------------------------------------------------------------------#
#         Blooming Forb and Weed Species ~ Rare Bee Species         #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence rare bee species richness?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and rare bee species richness
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(ggplot2)

#Read in data
BSonRB12 <- read.csv("mmackert/Data/Moorhouse Rare data set (AM).csv")
#Date = Date of sample
#Site = Site name
#Sampling = Sample period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015
#Quadrats = Combined coverage of blooming forb/weed species in ten quadrats
#SppBloomQ = Number of forb/weed species in bloom within ten quadrats
#BareGround = Average bare ground coverage in ten quadrats
#TotalAbundance = Total number of bees collected
#Total.Genus.Richness = Total number of bee genera collected
#Total.Species.Richness = Total number of bee species collected
#Following species names correspond to number of individuals collected of that species

#Change "year" to factor in BSonRB12
BSonRB12$Year <- as.factor(BSonRB12$Year)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB12model <- lm(Total.Rare.Species.Richness ~ SppBloomQ, data = BSonRB12)
summary(BSonRB12model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonRB12model)

#Plot: Number of blooming forb/weed species vs. rare bee species richness
BSonRB12plot <- ggplot(BSonRB12, aes(x = SppBloomQ, y = Total.Rare.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 7.3157307, slope = 0.9672395) +
  scale_color_manual(labels = c("2014", "2015"), values = c("darkorchid1", "darkgreen")) +
  scale_shape_manual(labels = c("2014", "2015"), values = c(15, 16)) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Rare Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Rare Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonRB12plot

#-------------------------------------------------------------------#
#         Blooming Forb and Weed Species ~ Rare Bee Species         #
#                             Years 3-4                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
BSonRB34 <- read.csv("mmackert/Data/Mackert Full data set.csv")

#Change "year" to a factor
BSonRB34$Year <- as.factor(BSonRB34$Year)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB34model <- lm(Total.Rare.Species.Richness ~ X..Blooming.species.in.quadrats, data = BSonRB34)
summary(BSonRB34model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonRB34model)

#####Missing year four species richness data
#Plot: Number of blooming forb/weed species vs. rare bee species richness
BSonRB34plot <- ggplot(BSonRB34, aes(x = X..Blooming.species.in.quadrats, y = Total.Rare.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 2.2036751, slope = 0.7091508) +
  scale_color_manual(labels = c("2016", "2017"), values = c("#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2016", "2017"), values = c(17, 18)) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Rare Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Rare Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonRB34plot

#-------------------------------------------------------------------#
#         Blooming Forb and Weed Species ~ Rare Bee Species         #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())

#Read in data
BSonRB123 <- read.csv("Data/years123.csv")

#Remove rows 132-171
BSonRB123 <- BSonRB123[-(132:171),]

#Change "year" to a factor
BSonRB123$Year <- as.factor(BSonRB123$Year)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB123model <- lm(Total.Rare.Species.Richness ~ X..Blooming.species.in.quadrats, data = BSonRB123)
summary(BSonRB123model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonRB123model)

#Plot: Number of blooming forb/weed species vs. rare bee species richness
BSonBS123plot <- ggplot(BSonRB123, aes(x = X..Blooming.species.in.quadrats, y = Total.Rare.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 7.1758069, slope = 0.8152224) +
  scale_color_manual(labels = c("2014", "2015", "2016"), values = c("darkorchid1", "darkgreen", "#000000")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"), values = c(15, 16, 17)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom", y = "Number of Rare Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Rare Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS123plot

#Plot as bar graph by sample period: Number of blooming forb/weed species vs. rare bee species richness
BSonBS123barplot <- ggplot(BSonRB123,
                           aes(x = Sampling.Period,
                               y = Total.Rare.Species.Richness,
                               fill = Site)) +
  geom_bar(stat = "identity",
           #position = "dodge",
           color = "black") +
  scale_fill_manual(values = c("darkorchid1", "darkgreen", "#000000", "blue", "red", "pink", "orange", "white", "purple", "cyan", "magenta")) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Rare Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Rare Bee Species Richness") +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS123barplot

Fulllandusebarcover <- ggplot(FullLandUseJoined,
                              aes(x = Site, y = Coverage, fill = LandType)) +
  geom_bar(stat = "identity",
           color = "black") +
  ggtitle("Land Use Surrounding Each Site \nWithin a 3km Radius") +
  theme_bw() +
  scale_fill_manual(values = barcolors) +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(legend.title.align = 0.5) +
  labs(y = "Coverage (sq. km)",
       fill = "Land Type")
Fulllandusebarcover