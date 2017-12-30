#-------------------------------------------------------------------#
#                             Bee Guilds                            #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#LOad libraries
library(lubridate)
library(dplyr)
library(ggplot2)

#Read in data
BeeIDs <- read.csv("Bees/Bee IDs.csv")
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Assign guild name to each specimen
BeeIDs123 <- BeeIDs123 %>%
  mutate(Guild = case_when(
    Binomial == "Bombus citrinus" ~ "Social parasite",
    Binomial == "Megachile latimanus" ~ "Solitary ground-nester",
    Genus == "Agapostemon" | Genus == "Andrena" | Genus == "Calliopsis" | Genus == "Colletes" | Genus == "Lasioglossum (Lasioglossum)" | Genus == "Melissodes" | Genus == "Pseudopanurgus" | Genus == "Perdita" | Genus == "Lasioglossum s.s." | Genus == "Lasioglossum"| Genus == "Eucera" | Genus == "Lasioglossum (Leuchalictus)" | Genus == "Lasioglossum (Hemihalictus)" | Genus == "Anthophora" | Genus == "Svastra" | Genus == "Nomia" | Genus == "Florilegus" | Genus == "Lasioglossum (Sphecodogastra)" | Genus == "Peponapis" | Genus == "Duforea" ~ "Solitary ground-nester",
    Genus == "Lasioglossum (Dialictus)" | Genus == "Lasioglossum (Evylaeus)" | Genus == "Augochlorella" | Genus == "Augochlora" | Genus == "Halictus" | Genus == "Augochloropsis" ~ "Social ground-nester",
    Genus == "Apis" ~ "Honey bee",
    Genus == "Bombus" ~ "Bumble bee",
    Genus == "Hoplitis" | Genus == "Hylaeus" | Genus == "Megachile" | Genus == "Osmia" | Genus == "Ceratina" | Binomial == "Anthophora terminalis" | Genus == "Xylocopa" | Genus == "Ashmeadiella" ~ "Cavity nester",
    Genus == "Coelioxys" | Genus == "Holcopasites" | Genus == "Nomada" | Genus == "Sphecodes" | Genus == "Triepeolus" ~ "Cleptoparasite"
  ))

#Create table showing the number of individuals within each guild by site and year
BeeIDs123byguildind <- BeeIDs123 %>%
  group_by(Site, Year, Guild) %>%
  count(Binomial)
BeeIDs123byguildind <- BeeIDs123byguildind %>%
  group_by(Site, Year, Guild) %>%
  summarise(Abundance = sum(n))

#Reformat from long to wide
BeeIDs123byguildindwide <- spread(BeeIDs123byguildind, Guild, Abundance)

#Fill NAs with 0
BeeIDs123byguildindwide[is.na(BeeIDs123byguildindwide)] <- 0

#Export as .csv
write.csv(BeeIDs123byguildindwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbyAbundance.csv")

#Create table showing the number of species in each guild by site and year
BeeIDs123byguildspecies <- BeeIDs123 %>%
  group_by(Site, Year, Guild) %>%
  summarise(NumberSpecies = length(unique(Binomial)))

#Reformat from long to wide format
BeeIDs123byguildspecieswide <- spread(BeeIDs123byguildspecies, Guild, NumberSpecies)

#Fill NAs with 0
BeeIDs123byguildspecieswide[is.na(BeeIDs123byguildspecieswide)] <- 0

#Export as .csv
write.csv(BeeIDs123byguildspecieswide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySpecies.csv")
  
#Create table showing number of individuals in each guild collected each year
BeeIDs123byguildyear <- BeeIDs123 %>%
  group_by(Year) %>%
  count(Guild)

#Change "Year" to a factor
BeeIDs123byguild$Year <- as.factor(BeeIDs123byguild$Year)

#Plot number of specimens collected in each guild by year
BeeIDs123byguildplot <- ggplot(BeeIDs123byguild,
                               aes(x = Guild,
                                   y = n)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 4) +
  theme_bw() + 
  labs(y = "Bee Abundance") +
  ggtitle("Number of Specimens Belonging to Each Guild by Year") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
BeeIDs123byguildplot

#Do the same but in a grouped bar plot
BeeIDs123byguildbarplot <- ggplot(BeeIDs123byguild,
                                  aes(x = Guild,
                                      y = n,
                                      fill = Year)) +
  geom_bar(color = "black",
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Number of Specimens Belonging to Each Guild by Year") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
BeeIDs123byguildbarplot

#Create table showing number of individuals collected at each site during each year
BeeIDs123byguildsite <- BeeIDs123 %>%
  group_by(Year, Site) %>%
  count(Guild)
BeeIDs123numberguilds <- BeeIDs123 %>%
  group_by(Year, Site) %>%
  summarise(NumberGuilds = length(unique(Guild)))

#Export "BeeIDs123byguildsite" to .csv to use in SAS analyses
write.csv(BeeIDs123byguildsite, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySite.csv")

#Graph that shiz
BeeIDs123byguildsiteplot <- ggplot(BeeIDs123byguildsite,
                                   aes(x = Guild,
                                       y = n)) +
  geom_point(aes(color = Site)) +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Number of Specimens Belonging to Each Guild by Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
BeeIDs123byguildsiteplot

#Bar graph
BeeIDs123byguildsitebarplot <- ggplot(BeeIDs123byguildsite,
                                      aes(x = Guild,
                                          y = n,
                                          fill = Site)) +
  geom_bar(color = "black",
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Number of Specimens Belonging to Each Guild by Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
BeeIDs123byguildsitebarplot
