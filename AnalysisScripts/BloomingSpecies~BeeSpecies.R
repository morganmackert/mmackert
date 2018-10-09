#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence bee species richness?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and bee species richness
#Use created model(s) to visualize the relationship graphically

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(vegan)
library(lme4)
library(multcompView)
library(ggplot2)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv")
Bees <- read.csv("Bees/Bee IDs.csv")

#Use lubridate to allow R to read the dates
Quadrats$Date <- mdy(Quadrats$Date)
Quadrats$Year <- year(Quadrats$Date)
Bees$Date <- mdy(Bees$Date)
Bees$Year <- year(Bees$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-2                             #
#-------------------------------------------------------------------#
#Subset Fulldata for only 2014-2015
years12 <- Fulldata %>%
  filter(Year <= 2015)

#Change year to factor
years12$Year <- as.factor(years12$Year)

#Model for bee abundance predicted by frequency of blooming species
BSonBS12model <- lm(Total.Species.Richness ~ Blooming.Species,
                    data = years12)
summary(BSonBS12model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS12plot <- ggplot(years12, aes(x = Blooming.Species,
                                     y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  scale_color_manual(labels = c("2014", "2015"),
                     values = c("darkorchid1", "#000000")) +
  scale_shape_manual(labels = c("2014", "2015"),
                     values = c(16, 17)) +
  labs(x = "Number of Blooming Species",
       y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BSonBS12plot

#Years 1-3 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Subset Fulldata for only years 2014-2016
years123 <- Fulldata %>%
  filter(Year <= 2016)

#Change year to factor
years123$Year <- as.factor(years123$Year)

#Model for bee species richness predicted by number of blooming species
BSonBS123model <- lm(Total.Species.Richness ~ Blooming.Species (1|Site) + (1|Sampling.Period) + (1|Year),
                     data = years123)
summary(BSonBS123model)

#Null model not including number of blooming species
BSonBS123null <- lmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                         data = years123)
summary(BSonBS123null)

#Likelihood ratio test between the full and null models
anova(BSonBS123null, BSonBS123model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS123plot <- ggplot(years123,
                        aes(x = Blooming.Species,
                            y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  scale_color_manual(labels = c("2014", "2015", "2016"),
                     values = c("darkorchid1", "#000000", "darkgreen")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"),
                     values = c(16, 17, 15)) +
  labs(x = "Number of Blooming Species",
       y = "Number of Bee Species") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BSonBS123plot

#Years 1-3 (-NS/E) ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#           Years 1-3, excluding NealSmith and Elkader              #
#-------------------------------------------------------------------#
#Subset Fulldata for only years 2014-2016
years123NSE <- Fulldata %>%
  filter(Site != "NealSmith") %>%
  filter(Site != "Elkader") %>%
  filter(Year <= 2016)

#Change year to factor
years123NSE$Year <- as.factor(years123NSE$Year)

#Model for bee species richness predicted by number of blooming species
BSonBS123NSEmodel <- lmer(Total.Species.Richness ~ Blooming.Species + (1|Site) + (1|Sampling.Period) + (1|Year),
                     data = years123NSE)
summary(BSonBS123NSEmodel)

#Null model not including number of blooming species
BSonBS123NSEnull <- lmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                       data = years123NSE)
summary(BSonBS123NSEnull)

#Likelihood ratio test between the full and null models
anova(BSonBS123NSEnull, BSonBS123NSEmodel)

#Plot of number of blooming forb/weed species vs. Bee species richness
BSonBS123NSEplot <- ggplot(years123NSE,
                        aes(x = Blooming.Species,
                            y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  scale_color_manual(labels = c("2014", "2015", "2016"),
                     values = c("darkorchid1", "#000000", "darkgreen")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"),
                     values = c(16, 17, 15)) +
  labs(x = "Number of Blooming Species",
       y = "Number of Bee Species") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BSonBS123NSEplot

#Year 4 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Year 4                                #
#-------------------------------------------------------------------#
#Subset Fulldata to include year 4
year4 <- Fulldata %>%
  filter(Year == 2017)

#Convert "Year" to a  factor
year4$Year <- as.factor(year4$Year)

#Model for bee species richness predicted by number of blooming species
BSonBS4model <- glmer(Total.Species.Richness ~ Blooming.Species + (1|Site) + (1|Sampling.Period),
                      data = year4,
                      family = "poisson")
summary(BSonBS4model)

#Null model not including number of blooming species
BSonBS4null <- glmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period),
                     data = year4,
                     family = "poisson")
summary(BSonBS4null)

#Likelihood ratio test between the full and null models
anova(BSonBS4null, BSonBS4model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BSonBS4model, residuals(BSonBS4model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonBS4model)

#Plot of the number of blooming forb/weed species vs. bee species richness
BSonBS4plot <- ggplot(year4, 
                         aes(x = Blooming.Species,
                             y = Total.Species.Richness)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species")
BSonBS4plot

#Years 1-4 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Read in bee identification data
BeeIDs <- read.csv("Bees/Bee IDs.csv")

#Format date and year in BeeIDs with lubridate
BeeIDs$Date <- mdy(BeeIDs$Date)
BeeIDs$Year <- year(BeeIDs$Date)

#Create a table to determine total number of species collected
BeeIDsTable <- BeeIDs %>%
  length(unique(Binomial))

#Subset Fulldata to include years 1-4
years1234 <- Fulldata %>%
  filter(Year <= 2017)

#Convert "Year" to a  factor
years1234$Year <- as.factor(years1234$Year)

#Model for bee species richness predicted by number of blooming species
BSonBS1234model <- lmer(Total.Species.Richness ~ Blooming.Species + (1|Site) + (1|Sampling.Period) + (1|Year),
                      data = years1234)
summary(BSonBS1234model)

#Null model not including number of blooming species
BSonBS1234null <- lmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                       data = years1234)
summary(BSonBS1234null)

#Likelihood ratio test between the full and null models
anova(BSonBS1234null, BSonBS1234model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BSonBS1234model, residuals(BSonBS1234model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonBS1234model)

#Plot of the number of blooming forb/weed species vs. bee species richness
BSonBS1234plot <- ggplot(years1234, 
                         aes(x = Blooming.Species,
                             y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS1234plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Read in bee identification data
BeeIDs <- read.csv("Bees/Bee IDs.csv")

#Format date and year in BeeIDs with lubridate
BeeIDs$Date <- mdy(BeeIDs$Date)
BeeIDs$Year <- year(BeeIDs$Date)

#Create a table to determine total number of species collected
BeeIDsTable <- BeeIDs %>%
  length(unique(Binomial))

#Subset Fulldata to include years 1-4
years1234 <- Fulldata %>%
  filter(Year <= 2017)

#Convert "Year" to a  factor
years1234$Year <- as.factor(years1234$Year)

#Model for bee species richness predicted by number of blooming species
BSonBS1234model <- lmer(Total.Species.Richness ~ Blooming.Species + (1|Site) + (1|Sampling.Period) + (1|Year),
                        data = years1234)
summary(BSonBS1234model)

#Null model not including number of blooming species
BSonBS1234null <- lmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                       data = years1234)
summary(BSonBS1234null)

#Likelihood ratio test between the full and null models
anova(BSonBS1234null, BSonBS1234model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BSonBS1234model, residuals(BSonBS1234model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonBS1234model)

#Plot of the number of blooming forb/weed species vs. bee species richness
BSonBS1234plot <- ggplot(years1234, 
                         aes(x = Blooming.Species,
                             y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS1234plot

#Data dictionary ####
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
#Old code ####
#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"