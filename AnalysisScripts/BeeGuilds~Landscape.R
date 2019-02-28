#-------------------------------------------------------------------#
#                 Bee Guilds ~ Surrounding Landscape                #
#-------------------------------------------------------------------#

#Research Question:  Do the number of bee species/individuals vary with the surrounding landscape?

#Objective:  Perform generalized linear mixed models with bee metrics as the response variable and landscape type as the predictor variable.

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(lme4)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = TRUE, na.strings = c("", "NA"))
FullLandUse <- read.csv("Sites/FullLandUse.csv")

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)

#Add new column with only the year
Bees$Year <- year(Bees$Date)

#Subset BeeIDs without target bees, wasps, or unidentifiable specimens
bees <- Bees %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site))

#Assign guild name to each specimen
bees <- bees %>%
  mutate(Guild = case_when(
    Binomial == "Bombus citrinus" ~ "Social parasite",
    Binomial == "Megachile latimanus" ~ "Solitary ground-nester",
    Genus == "Agapostemon" | Genus == "Andrena" | Genus == "Calliopsis" | Genus == "Colletes" | Genus == "Dieunomia" | Genus == "Melissodes" | Genus == "Pseudopanurgus" | Genus == "Perdita" | Genus == "Lasioglossum" | Genus == "Eucera" | Genus == "Anthophora" | Genus == "Svastra" | Genus == "Nomia" | Genus == "Florilegus" | Genus == "Peponapis" | Genus == "Dufourea" | Genus == "Protandrena" | Genus == "Lasioglossum (Evylaeus)" ~ "Solitary ground-nester",
    Genus == "Lasioglossum (Dialictus)" | Genus == "Augochlorella" | Genus == "Augochlora" | Genus == "Halictus" | Genus == "Augochloropsis" ~ "Social ground-nester",
    Genus == "Apis" ~ "Honey bee",
    Genus == "Bombus" ~ "Bumble bee",
    Genus == "Heriades" | Genus == "Hoplitis" | Genus == "Hylaeus" | Genus == "Megachile" | Genus == "Osmia" | Genus == "Ceratina" | Binomial == "Anthophora terminalis" | Genus == "Xylocopa" | Genus == "Ashmeadiella" ~ "Cavity nester",
    Genus == "Coelioxys" | Genus == "Holcopasites" | Genus == "Nomada" | Genus == "Sphecodes" | Genus == "Stelis" | Genus == "Triepeolus" | Genus == "Xeromelecta" | Genus == "Epeolus" ~ "Cleptoparasite"
  ))

#Check to make sure all species have been assigned a guild
bees %>% 
  group_by(Guild) %>% 
  tally()
#Good!

#Assign number to each site
FullLandUse <- FullLandUse %>%
  mutate(SiteNum = ifelse(Site == "Bowman", "1",
                          ifelse(Site == "Cretsinger", "2",
                                 ifelse(Site == "Elkader", "3",
                                        ifelse(Site == "Greving", "4",
                                               ifelse(Site == "Kaldenberg", "5",
                                                      ifelse(Site == "McClellan", "6",
                                                             ifelse(Site == "NealSmith", "7",
                                                                    ifelse(Site == "Peckumn", "8",
                                                                           ifelse(Site == "Plunkett", "9",
                                                                                  ifelse(Site == "Sheller", "10",
                                                                                         ifelse(Site == "Sloan", "11",
                                                                                                NA
                                                                                         ))))))))))))

#Determine the number of bees collected in each guild at each site during each date
bees.guild.site <- bees %>%
  group_by(Site, Guild) %>%
  count(Binomial)
bees.guild.site <- bees.guild.site %>%
  group_by(Site, Guild) %>%
  summarise(no.bees = sum(n))

#Determine the number of bee species collected in each guild at each site
beespp.guild.site <- bees %>%
  group_by(Site, Guild) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Join the datasets together
beesguild.landuse <- left_join(bees.guild.site, FullLandUse, by = "Site")
beesppguild.landuse <- left_join(beespp.guild.site, FullLandUse, by = "Site")

#Use facet_grid to graph nesting guilds vs. land type for bee abundance
beesguild.landuse.plot <- ggplot(beesguild.landuse,
                                 aes(x = Coverage,
                                     y = no.bees)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_grid(LandType ~ Guild) +
  theme_bw() +
  labs(x = "Coverage (sq. km.)",
       y = "Bee Abundance")
beesguild.landuse.plot

#Use facet_grid to graph nesting guilds vs. land type for bee species richness
beesppguild.landuse.plot <- ggplot(beesppguild.landuse,
                                   aes(x = Coverage,
                                       y = no.beespp)) +
  geom_point() +
  geom_smooth(method = "glm") +
  facet_grid(LandType ~ Guild) +
  theme_bw() +
  labs(x = "Coverage (sq. km)",
       y = "Bee Species Richness")
beesppguild.landuse.plot
 