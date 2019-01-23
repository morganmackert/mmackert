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

#Filter out bad stuff from Bees
bees <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(!is.na(Site))

#Format Bare.Ground column as numeric
Quadrats$BareGround <- as.numeric(Quadrats$BareGround)

#Average bare ground cover for each site
bareground <- Quadrats %>%
  filter(!is.na(BareGround)) %>%
  group_by(Site, Date, Quadrat) %>%
  summarise(total.bareground = BareGround[1])
avg.bareground <- bareground %>%
  group_by(Site) %>%
  summarise(avg.bareground = mean(total.bareground), 
            number.quadrats = length(total.bareground))

#Average floral cover for each site
floral.cover <- Quadrats %>%
  group_by(Site) %>%
  summarise(avg.floralcover = mean(Cover))

#Determine species richness without nesting plots
beespp.noplot <- Bees %>%
  filter(!is.na(Binomial)) %>%
  filter(Family != "Wasp") %>%
  filter(Trap != "Plot") %>%
  count(Binomial)

#Bee species richness
beespp <- Bees %>%
  filter(!is.na(Binomial)) %>%
  filter(Family != "Wasp") %>%
  count(Binomial)

#Determine abundance for each site
bees.site <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  count(Binomial)
bees.site <- bees.site %>%
  group_by(Site) %>%
  summarise(total.bees = sum(n))

#Check to make sure total matches original datafile
bees.site %>%
  summarise(sum(total.bees))
#15,904, good to go!

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
beespp.site <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  summarise(no.beespp = n_distinct(Binomial))

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

#Genus richness for 2014-2018
genusrich <- Bees %>%
  group_by(Genus) %>%
  count(Genus)
