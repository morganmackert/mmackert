#-------------------------------------------------------------------#
#                  Summary Statistics for Full Dataset              #
#                              2014-2018                            #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = T, na.strings = c("", "NA"))
Quadrats <- read.csv("Plants/Quadrats.csv")

#Format date with lubridate
Bees$Date <- mdy(Bees$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Change Year from number to year
Bees$Year <- year(Bees$Date)
Quadrats$Year <- year(Quadrats$Date)

#Format Bare.Ground column as numeric
Quadrats$BareGround <- as.numeric(Quadrats$BareGround)

#Determine average bare ground coverage for each site
AverageBareGround <- Quadrats %>%
  select(Date, Site, Quadrat, BareGround) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(BareGround = BareGround[1]) %>%
  group_by(Site) %>%
  summarise(Average.Bare.Ground = mean(BareGround))

#Determine abundance for each site
AbundSite <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(!is.na(Binomial)) %>%
  group_by(Site) %>%
  count(Binomial)
AbundSite <- AbundSite %>%
  group_by(Site) %>%
  summarise(Bee.Abundance = sum(n))

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
#write.csv(AbundTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/AbundancebyTrap1234.csv")

#Determine species richness for each site
SpeciesRichnessSite <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site) %>%
  summarise(length(unique(Binomial)))

#Determine species richness by trap for each site/date
SpecRichTrap <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date, Trap) %>%
  summarise(Bee.Species.Richness = length(unique(Binomial)))
SpecRichTrapwide <- spread(SpecRichTrap, Trap, Bee.Species.Richness)

#Export as .csv
#write.csv(SpecRichTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/SpeciesRichnessbyTrap1234.csv")

#Determine genus richness for each site/date
GenusRich <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  summarise(Bee.Genus.Richness = length(unique(Genus)))

#Export as .csv
#write.csv(GenusRich, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/GenusRichness1234.csv")

#Determine number of individuals of each species collected for each site/date
SpecRichAbund <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  count(Binomial)
SpecRichAbundwide <- spread(SpecRichAbund, Binomial, n)
SpecRichAbundwide[is.na(SpecRichAbundwide)] <- 0

#Export as .csv
#write.csv(SpecRichAbundwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/SpeciesRichnessandAbundance1234.csv")