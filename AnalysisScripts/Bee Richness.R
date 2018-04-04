#-------------------------------------------------------------------#
#                  Summary Statistics for Full Dataset              #
#                                2017                               #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Read in data
BeeIDs <- read.csv("Bees/Bee IDs.csv")

#Format date with lubridate
BeeIDs$Date <- mdy(BeeIDs$Date)

#Change year from number to year
BeeIDs$Year <- year(BeeIDs$Date)

#Determine abundance by trap for each site/date
AbundTrap <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date, Trap) %>%
  count(Binomial)
AbundTrap <- AbundTrap %>%
  group_by(Site, Date, Trap) %>%
  summarise(Bee.Abundance = sum(n))
AbundTrapwide <- spread(AbundTrap, Trap, Bee.Abundance)

#Export as .csv
write.csv(AbundTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness/AbundancebyTrap1234.csv")

#Determine species richness by trap for each site/date
SpecRichTrap <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date, Trap) %>%
  summarise(Bee.Species.Richness = length(unique(Binomial)))
SpecRichTrapwide <- spread(SpecRichTrap, Trap, Bee.Species.Richness)

#Export as .csv
write.csv(SpecRichTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness/SpeciesRichnessbyTrap1234.csv")

#Determine genus richness for each site/date
GenusRich <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  summarise(Bee.Genus.Richness = length(unique(Genus)))

#Export as .csv
write.csv(GenusRich, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness/GenusRichness1234.csv")

#Determine number of individuals of each species collected for each site/date
SpecRichAbund <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  count(Binomial)
SpecRichAbundwide <- spread(SpecRichAbund, Binomial, n)

#Export as .csv
write.csv(SpecRichAbundwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeRichness/SpeciesRichnessandAbundance1234.csv")

