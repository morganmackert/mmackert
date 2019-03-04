#-------------------------------------------------------------------#
#         Pairwise Correlation of Bee and Flower Species            #
#-------------------------------------------------------------------#

#Research Question: Are there certain floral species that impact bee species more than others?

#Objectives:
#Conduct pairwise comparison of all bee species compared to all floral species to see if any floral species are more important than others

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
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

#Determine total number of individuals in each bee species collected throughout the study and filter out singletons/doubletons
bees.spp <- bees %>%
  group_by(Site, Date, Genus) %>%
  count(Binomial) %>%
  filter(n >= 3)

#Subset Apidae from bee.spp
apidae <- bees.spp %>%
  filter(Family == "Apidae")

#Subset Bombus from bee.spp
bombus <- bees.spp %>%
  filter(Genus == "Bombus")

#Determine average coverage of each floral species during each sampling event
floral.cover <- Quadrats %>%
  filter(!is.na(Species)) %>%
  group_by(Site, Date, Quadrat, Species) %>%
  summarise(floral.cover = sum(Cover))
floral.cover <- floral.cover %>%
  group_by(Site, Date, Species) %>%
  summarise(avg.floralcover = mean(floral.cover))

#Join the two datasets together using "right_join" to include only bee species in the presence of floral species
floralcover.beespp <- right_join(bees.spp, floral.cover, by = c("Site", "Date"))

#Join apidae and floral.cover together
floralcover.apidae <- right_join(apidae, floral.cover, by = c("Site", "Date"))

#Remove NAs from floralcover.apidae
floralcover.apidae <- na.omit(floralcover.apidae)

#Join apidae and floral.cover together
floralcover.bombus <- right_join(bombus, floral.cover, by = c("Site", "Date"))

#Remove NAs from floralcover.apidae
floralcover.bombus <- na.omit(floralcover.bombus)

#Filter floralcover.bombus
floralcover.bombus2 <- floralcover.bombus %>%
  filter(Species != "Ashy sunflower") %>%
  filter(Species != "Dogbane") %>%
  filter(Species != "Dotted smartweed") %>%
  filter(Species != "Hoary vervain") %>%
  filter(Species != "Marestail") %>%
  filter(Species != "New England aster") %>%
  filter(Species != "Pennsylvania smartweed") %>%
  filter(Species != "Purple prairie clover") %>%
  filter(Species != "Rattlesnake master") %>%
  filter(Species != "Whorled milkweed")

#Fill in 0 for any NAs in Total.Abundance (showing we sampled vegetation, but collected no bees)
floralcover.beespp$n[is.na(floralcover.beespp$n)] <- 0

#Include Year column in floralcover.bees using lubridate
floralcover.beespp$Year <- year(floralcover.beespp$Date)

#Use facet_grid to graph bee species vs. floral species
floralcover.beespp.plot <- ggplot(floralcover.beespp,
                                  aes(x = avg.floralcover,
                                      y = n)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_grid(Species ~ Binomial) +
  theme_bw() +
  labs(x = "Floral Cover (%)",
       y = "Bee Abundance")
floralcover.beespp.plot

floralcover.bombus.plot <- ggplot(floralcover.bombus2,
                                  aes(x = avg.floralcover,
                                      y = n)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_grid(Binomial ~ Species) +
  theme_bw() +
  labs(x = "Floral Cover (%)",
       y = "Bee Abundance")
floralcover.bombus.plot
