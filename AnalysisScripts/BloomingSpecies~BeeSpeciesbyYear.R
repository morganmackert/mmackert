#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence bee species richness for all years of the study?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and bee species richness
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(lubridate)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv", header = TRUE, na.strings = c("", "NA"))
Bees <- read.csv("Bees/Bee IDs.csv", header = TRUE, na.strings = c("", "NA"))

#Use lubridate to allow R to read the dates
Quadrats$Date <- mdy(Quadrats$Date)
Quadrats$Year <- year(Quadrats$Date)
Bees$Date <- mdy(Bees$Date)
Bees$Year <- year(Bees$Date)

#Fill NAs with 0 in Quadrats$Cover to indicate no plants were blooming at that point
Quadrats$Cover[is.na(Quadrats$Cover)] <- 0

#Filter out icky stuff from Bees
bees <- Bees %>%
  filter(Binomial != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(Family != "Wasp") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Determine total number of bee species collected at each site during each collection event
bees.spp <- bees %>%
  group_by(Site, Date) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Determine number of plant species in bloom for each site/date
floral.species <- Quadrats %>%
  group_by(Site, Date) %>%
  summarise(no.floralspp = n_distinct(Species, na.rm = TRUE))

#Join the two datasets together
floralspp.beespp <- full_join(bees.spp, floral.species, by = c("Site", "Date"))

#Fill in 0 for any NAs in Total.Abundance (showing we sampled vegetation, but collected no bees)
floralspp.beespp$no.beespp[is.na(floralspp.beespp$no.beespp)] <- 0

#Include Year column in floralcover.bees using lubridate
floralspp.beespp$Year <- year(floralspp.beespp$Date)

#2014 ####
#---------------------------#
#           2014            #
#---------------------------#
#Subset floralspp.beespp to include only 2014
floralspp.beespp.2014 <- floralspp.beespp %>%
  filter(Year == 2014)

#Models
BSonBS2014model <- lmer(no.beespp ~ no.floralspp + (1|Site) + (1|Date),
                        data = floralspp.beespp.2014)
summary(BSonBS2014model)
AIC(BSonBS2014model)
#AIC = 241.7581; p-value = 0.0749

BSonBS2014model2 <- lmer(no.beespp ~ no.floralspp + (1|Site),
                        data = floralspp.beespp.2014)
summary(BSonBS2014model2)
AIC(BSonBS2014model2)
#AIC = 239.7581; p-value = 0.0749

BSonBS2014model3 <- lmer(no.beespp ~ no.floralspp + (1|Date),
                        data = floralspp.beespp.2014)
summary(BSonBS2014model3)
AIC(BSonBS2014model3)
#AIC = 254.8954; p-value < 0.001

#Graph
BSonBS2014 <- ggplot(floralspp.beespp.2014,
                     aes(x = no.floralspp,
                         y = no.beespp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness in 2014") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5))
BSonBS2014

#2015 ####
#---------------------------#
#           2015            #
#---------------------------#
#Subset floralspp.beespp to include only 2015
floralspp.beespp.2015 <- floralspp.beespp %>%
  filter(Year == 2015)

#Models
BSonBS2015model <- lmer(no.beespp ~ no.floralspp + (1|Site) + (1|Date),
                        data = floralspp.beespp.2015)
summary(BSonBS2015model)
AIC(BSonBS2015model)
#AIC = 296.817; p-value = 0.157

BSonBS2015model2 <- lmer(no.beespp ~ no.floralspp + (1|Site),
                         data = floralspp.beespp.2015)
summary(BSonBS2015model2)
AIC(BSonBS2015model2)
#AIC = 295.7718; p-value = 0.178

BSonBS2015model3 <- lmer(no.beespp ~ no.floralspp + (1|Date),
                         data = floralspp.beespp.2015)
summary(BSonBS2015model3)
AIC(BSonBS2015model3)
#AIC = 305.3723; p-value = 0.0297

#Graph
BSonBS2015 <- ggplot(floralspp.beespp.2015,
                     aes(x = no.floralspp,
                         y = no.beespp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness in 2015") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5))
BSonBS2015

#2016 ####
#---------------------------#
#           2016            #
#---------------------------#
#Subset floralspp.beespp to include only 2016
floralspp.beespp.2016 <- floralspp.beespp %>%
  filter(Year == 2016)

#Models
BSonBS2016model <- lmer(no.beespp ~ no.floralspp + (1|Site) + (1|Date),
                        data = floralspp.beespp.2016)
summary(BSonBS2016model)
AIC(BSonBS2016model)
#AIC = 251.5772; p-value = 0.000451

BSonBS2016model2 <- lmer(no.beespp ~ no.floralspp + (1|Site),
                         data = floralspp.beespp.2016)
summary(BSonBS2016model2)
AIC(BSonBS2016model2)
#AIC = 261.8682; p-value = 0.000991

BSonBS2016model3 <- lmer(no.beespp ~ no.floralspp + (1|Date),
                         data = floralspp.beespp.2016)
summary(BSonBS2016model3)
AIC(BSonBS2016model3)
#AIC = 249.5772; p-value = 0.000451

#Graph
BSonBS2016 <- ggplot(floralspp.beespp.2016,
                     aes(x = no.floralspp,
                         y = no.beespp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness in 2016") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5))
BSonBS2016

#2017 ####
#---------------------------#
#           2017            #
#---------------------------#
#Subset floralspp.beespp to include only 2017
floralspp.beespp.2017 <- floralspp.beespp %>%
  filter(Year == 2017)

#Models
BSonBS2017model <- lmer(no.beespp ~ no.floralspp + (1|Site) + (1|Date),
                        data = floralspp.beespp.2017)
summary(BSonBS2017model)
AIC(BSonBS2017model)
#AIC = 251.2189; p-value < 0.0001

BSonBS2017model2 <- lmer(no.beespp ~ no.floralspp + (1|Site),
                         data = floralspp.beespp.2017)
summary(BSonBS2017model2)
AIC(BSonBS2017model2)
#AIC = 250.2061; p-value < 0.0001

BSonBS2017model3 <- lmer(no.beespp ~ no.floralspp + (1|Date),
                         data = floralspp.beespp.2017)
summary(BSonBS2017model3)
AIC(BSonBS2017model3)
#AIC = 249.82; p-value < 0.0001

#Graph
BSonBS2017 <- ggplot(floralspp.beespp.2017,
                     aes(x = no.floralspp,
                         y = no.beespp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness in 2017") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5))
BSonBS2017

#2018 ####
#---------------------------#
#           2018            #
#---------------------------#
#Subset floralspp.beespp to include only 2018
floralspp.beespp.2018 <- floralspp.beespp %>%
  filter(Year == 2018)

#Models
BSonBS2018model <- lmer(no.beespp ~ no.floralspp + (1|Site) + (1|Date),
                        data = floralspp.beespp.2018)
summary(BSonBS2018model)
AIC(BSonBS2018model)
#AIC = 251.4975; p-value < 0.0001

BSonBS2018model2 <- lmer(no.beespp ~ no.floralspp + (1|Site),
                         data = floralspp.beespp.2018)
summary(BSonBS2018model2)
AIC(BSonBS2018model2)
#AIC = 251.9567; p-value = 0.000394

BSonBS2018model3 <- lmer(no.beespp ~ no.floralspp + (1|Date),
                         data = floralspp.beespp.2018)
summary(BSonBS2018model3)
AIC(BSonBS2018model3)
#AIC = 249.4975; p-value < 0.0001

#Graph
BSonBS2018 <- ggplot(floralspp.beespp.2018,
                     aes(x = no.floralspp,
                         y = no.beespp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness in 2018") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5))
BSonBS2018
