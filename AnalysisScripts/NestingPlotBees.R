#-------------------------------------------------------------------#
#                          Nesting Plot Bees                        #
#-------------------------------------------------------------------#

#Research Question:  If we provide bare soil areas within flight distance of strips, will ground-nesting bees utilize them?

#Objectives
#Quantify the number of individual bees and bee species collected from the nesting plots
#Create ternary plot of bee species and their individual soil preferences

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggtern)
library(plyr)


#Read in data
NPbees <- read.csv("Bees/2017-2018 Nesting Plot Bees.csv")
soils <- read.csv("Soil/Reduced Analysis Results.csv")

#Use lubridate to allow R to recognize the dates
NPbees$Date <- mdy(NPbees$Date)

#Add new column with only the year
NPbees$Year <- year(NPbees$Date)

#Filter out "wasp" entries from NPBees
NPbees <- NPbees %>%
  filter(Family != "Wasp")

#Rename columns in "Reduced Analysis Results"
colnames(soils) <- c("Sample ID", "Site", "Depth", "Sand", "Silt", "Clay", "Texture")

#Extract plot number from "Sample.ID" in soils into separate column
soils$Plot <- substr(as.vector(soils$"Sample ID"), 3, 3)

#Convert "Plot" to integer in soils
soils$Plot <- as.integer(soils$Plot)

#Only include 0-6 inches of soil depth in ternary graph
soils <- soils %>%
  filter(Depth == "0 - 6")

#Year 4 ####
#-------------------------------------------------------------------#
#                               Year 4                              #
#-------------------------------------------------------------------#

#Subset only 2017 bees, not including wasps or unidentifiable specimens
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017) %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site))

#Apply county names to corresponding sites (ugly but it works)
BeeIDs1234 <- BeeIDs1234 %>%
  mutate(County = ifelse(Site == "Plunkett", "Story",
                         ifelse(Site == "Bowman", "Dallas",
                                ifelse(Site == "Kaldenberg", "Jasper",
                                       ifelse(Site == "McClellan", "Jasper",
                                              ifelse(Site == "Sloan", "Buchanan",
                                                     ifelse(Site == "Sheller", "Grundy",
                                                            ifelse(Site == "Cretsinger", "Guthrie",
                                                                   ifelse(Site == "Peckumn", "Greene",
                                                                          ifelse(Site == "Greving", "Carroll",
                                                                                 ifelse(Site == "NealSmith", "Jasper",
                                                                                        ifelse(Site == "Elkader", "Clayton",
                                                                                               NA
                                                                                        ))))))))))))
#Apply RRW (yes or no) to counties
BeeIDs1234 <- BeeIDs1234 %>%
  mutate(RRW = ifelse(County == "Story", "Non-RRW",
                      ifelse(County == "Dallas", "RRW",
                             ifelse(County == "Jasper", "Non-RRW",
                                    ifelse(County == "Buchanan", "Non-RRW",
                                           ifelse(County == "Grundy", "Non-RRW",
                                                  ifelse(County == "Guthrie", "RRW",
                                                         ifelse(County == "Greene", "RRW",
                                                                ifelse(County == "Carroll", "RRW",
                                                                       ifelse(County == "Clayton", "Non-RRW",
                                                                              NA
                                                                       ))))))))))

#Subset only nesting plot bees
BeeIDs1234NP <- BeeIDs1234 %>%
  filter(Trap == "Plot")

#Determine number of bees collected in RRW and non-RRW sites
BeeIDs1234RRWNP <- BeeIDs1234NP %>%
  group_by(RRW) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site and date
BeeIDs1234NPbysitedate <- BeeIDs1234NP %>%
  group_by(Site, Date) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site
BeeIDs1234NPbysite <- BeeIDs1234NP %>%
  group_by(Site) %>%
  count(Binomial)

#Table describing the number of species collected at each site
BeeIDs1234NPsppbysite <- BeeIDs1234NP %>%
  group_by(Site) %>%
  summarise(Total.Richness = length(unique(Binomial)))

#Years 4-5 ####
#-------------------------------------------------------------------#
#                            Years 4-5                              #
#-------------------------------------------------------------------#
#Determine total number of species
NPbeesspp <- NPbees %>%
  group_by(Binomial) %>%
  count()

#Determine total number of individuals
sum(NPbeesspp$n)
#330

#Which sites were they collected from?
NPbeesbysite <- NPbees %>%
  group_by(Site) %>%
  count(Binomial)

#How do the numbers differ between years?
NPbeesbyyear <- NPbees %>%
  group_by(Year) %>%
  count(Binomial)

NPbeesnobyyear <- NPbeesbyyear %>%
  group_by(Year) %>%
  summarise(sum = sum(n))

#Combine NPbees and soils data frames
soils.NPbees <- left_join(soils, NPbees, by = c("Site", "Plot"))

#Lasioglossum (Dialictus) sp. and Halictus confusus both occurr at all sites; remove for greater clarity in ternary plot
soils.NPbees.red <- soils.NPbees %>%
  filter(Binomial != "Lasioglossum (Dialictus) sp.") %>%
  filter(Binomial != "Halictus confusus")

#Set color scheme for plot
familycolors = c("#000000", "red3", "darkgreen", "goldenrod", "cyan")

#Graph bee families on soils ternary plot
soils.NPbeesfamilytern <- ggtern(data = soils.NPbees,
                                 aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Family),
             shape = 21,
             size = 2,
             position = position_jitter_tern(x = 0.1, y = 0.1, z = 0.1),
             color = "black") +
  geom_point(data = subset(soils.NPbees, Family == "Megachilidae")) +
  #geom_mean_ellipse(aes(color = Family)) +
  ggtitle("Soil Composition Used by \nBee Families in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = familycolors,
                                                  color = "black"))) +
  scale_fill_manual(values = familycolors)
soils.NPbeesfamilytern

#Graph bee species names on soils ternary plot
soils.NPbeestern <- ggtern(data = soils.NPbees.red,
                           aes(x = Sand, y = Silt, z = Clay)) +
  #geom_point(aes(color = Binomial))
  geom_text(aes(label = Binomial),
            position = position_jitter_tern(x = 0.5, y = 0.5, z = 0.5)) +
  ggtitle("Soil Composition Used by \nBee Species in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15))
soils.NPbeestern

#By Family ####
#-------------------------------------------------------------------#
#                            By Family                              #
#-------------------------------------------------------------------#
#Andrenidae
#Filter soils.NPbees to include only Andrenidae
soils.NPbeesAnd <- soils.NPbees %>%
  filter(Family == "Andrenidae")

#Set color scheme for plot
andcolors = c("#000000", "red3", "goldenrod", "darkgreen")

#Graph Andrenidae
soils.NPbeesAndtern <- ggtern(data = soils.NPbeesAnd,
                              aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Binomial),
             position = position_jitter_tern(x = 0.15, y = 0.15, z = 0.15),
             shape = 21,
             size = 2,
             color = "black") +
  #geom_text(aes(label = Binomial)) +
  ggtitle("Soil Composition Used by \nAndrenidae in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = andcolors,
                                                  color = "black"))) +
  scale_fill_manual(values = andcolors)
soils.NPbeesAndtern

#Apidae
#Filter soils.NPbees to include only Apidae
soils.NPbeesApi <- soils.NPbees %>%
  filter(Family == "Apidae")

#Set color scheme for plot
apicolors <-c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Graph Apidae
soils.NPbeesApitern <- ggtern(data = soils.NPbeesApi,
                              aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Binomial),
             position = position_jitter_tern(x = 0.1, y = 0.1, z = 0.1),
             shape = 21,
             size = 2,
             color = "black") +
  #geom_text(aes(label = Binomial)) +
  ggtitle("Soil Composition Used by \nApidae in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = apicolors,
                                                  color = "black"))) +
  scale_fill_manual(values = apicolors)
soils.NPbeesApitern

#Colletidae
#Filter soils.NPbees to include only Colletidae
soils.NPbeesCol <- soils.NPbees %>%
  filter(Family == "Colletidae")

#Set color scheme for plot
colcolors <-c("goldenrod", "red4")

#Graph Colletidae
soils.NPbeesColtern <- ggtern(data = soils.NPbeesCol,
                              aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Binomial),
             position = position_jitter_tern(x = 0.12, y = 0.12, z = 0.12),
             shape = 21,
             size = 3,
             color = "black") +
  #geom_text(aes(label = Binomial)) +
  ggtitle("Soil Composition Used by \nColletidae in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = colcolors,
                                                  color = "black"))) +
  scale_fill_manual(values = colcolors)
soils.NPbeesColtern

#Halictidae
#Filter soils.NPbees to include only Halictidae
soils.NPbeesHal <- soils.NPbees %>%
  filter(Family == "Halictidae")

#Set color scheme for plot
halcolors <-c("#000000", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

#Graph Halictidae
soils.NPbeesHaltern <- ggtern(data = soils.NPbeesHal,
                              aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Binomial),
             position = position_jitter_tern(x = 0.15, y = 0.15, z = 0.15),
             shape = 21,
             size = 3,
             color = "black") +
  #geom_text(aes(label = Binomial)) +
  ggtitle("Soil Composition Used by \nHalicidate in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = halcolors,
                                                  color = "black"))) +
  scale_fill_manual(values = halcolors)
soils.NPbeesHaltern

#Megachilidae
#Filter soils.NPbees to include only Megachilidae
soils.NPbeesMeg <- soils.NPbees %>%
  filter(Family == "Megachilidae")

#Set color scheme for plot
megcolors <-c("#000000", "red3", "goldenrod")

#Graph Halictidae
soils.NPbeesMegtern <- ggtern(data = soils.NPbeesMeg,
                              aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(fill = Binomial),
             position = position_jitter_tern(x = 0.1, y = 0.1, z = 0.1),
             shape = 21,
             size = 3,
             color = "black") +
  #geom_text(aes(label = Binomial)) +
  ggtitle("Soil Composition Used by \nMegachilidae in Nesting Plots") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = megcolors,
                                                  color = "black"))) +
  scale_fill_manual(values = megcolors)
soils.NPbeesMegtern

#Data dictionary ####
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet