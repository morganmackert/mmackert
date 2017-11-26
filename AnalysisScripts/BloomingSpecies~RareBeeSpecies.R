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
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(ggplot2)

#Read in data
Fulldata <- read.csv("Combined full data set.csv")
#Date = Date of sample
#Site = Site name
#Sampling.Period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#X..Floral.Cover..in.10m2. = Average coverage of blooming forb/weed species in ten quadrats
#X..Blooming.species.in.quadrats = Number of forb/weed species in bloom within ten quadrats
#X..Bare.Ground..in.10m2. = Average bare ground coverage in ten quadrats
#Trapname.Abundance = Number of individual bees collected by specified trap/site/date
#Total.Abundance = Number of individual bees collected by all trap types at the specified site/date
#Trapname.Species.Richness = Number of bee species collected by specified trap/site/date
#Total.Species.Richness = Number of bee species collected by all trap types at the specified site/date
#Species.Name = Number of individuals of specified species collected at the specified site/date

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset only years 1-2
BSonRB12 <- filter(Fulldata, Year <= 2)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB12model <- lm(Total.Rare.Species.Richness ~ Blooming.Species, data = BSonRB12)
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
#Subset for years 3-4
BSonRB34 <- filter(Fulldata, Year >= 3)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB34model <- lm(Total.Rare.Species.Richness ~ Blooming.Species, data = BSonRB34)
summary(BSonRB34model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonRB34model)

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
#Subset for years 1-3
BSonRB123 <- filter(Fulldata, Year <= 3)

#Model for influence of number of blooming species on number of rare bee species present (rare meaning 0<x<10)
BSonRB123model <- lm(Total.Rare.Species.Richness ~ Blooming.Species, data = BSonRB123)
summary(BSonRB123model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonRB123model)

#Set Year to factor for graphing purposes
BSonRB123$Year <- as.factor(BSonRB123$Year)

#Plot: Number of blooming forb/weed species vs. rare bee species richness
BSonRB123plot <- ggplot(BSonRB123,
                         aes(x = Blooming.Species,
                             y = Total.Rare.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016"),
                     values = c("darkorchid1", "darkgreen", "#000000")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"),
                     values = c(15, 16, 17)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Rare Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Rare Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonRB123plot

#Plot as bar graph by sample period: Number of blooming forb/weed species vs. rare bee species richness
#Load in other data set
RBSbySP123 <- read.csv("Data/Bees/Rare Bees by Sample Period.csv")
BSonRBS123barplot <- ggplot(RBSbySP123,
                           aes(x = Sample.Period,
                               y = Number.Rare.Bee.Species,
                               fill = Site)) +
  geom_bar(stat = "identity",
           color = "black") +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  labs(x = "Sample Period",
       y = "Number of Rare Bee Species") +
  ggtitle("The Number of Rare Bee Species Collected \nDuring Each Sample Period from 2014 to 2016") +
  theme(plot.title = element_text(size = 15, 
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonRBS123barplot

#Model for influence of number of blooming species on number of bee species represented by only one individual
BSon1Bmodel123 <- lm(Species.with.One.Individual ~ Blooming.Species,
                     data = BSonRB123)
summary(BSon1Bmodel123)
