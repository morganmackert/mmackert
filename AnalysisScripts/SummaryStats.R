#-------------------------------------------------------------------#
#                  Summary Statistics for Full Dataset              #
#                              2014-2017                            #
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
Quadrats <- read.csv("Plants/Quadrats.csv")

#Format date with lubridate
BeeIDs$Date <- mdy(BeeIDs$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Change Year from number to year
BeeIDs$Year <- year(BeeIDs$Date)
Quadrats$Year <- year(Quadrats$Date)

#Change column names so they're not so DUMB
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Floral.Cover"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Species"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"

#Format Bare.Ground column as numeric
Quadrats$Bare.Ground <- as.numeric(Quadrats$Bare.Ground)

#Determine average bare ground coverage for each site
AverageBareGround <- Quadrats %>%
  select(Date, Site, Quadrat, Bare.Ground) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(Bare.Ground = Bare.Ground[1]) %>%
  group_by(Site) %>%
  summarise(Average.Bare.Ground = mean(Bare.Ground))

#Determine abundance for each site
AbundSite <- BeeIDs %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
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

