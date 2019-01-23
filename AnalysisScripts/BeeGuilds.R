#-------------------------------------------------------------------#
#                             BEE GUILDS                            #
#-------------------------------------------------------------------#

#Research Question:  How do bee nesting guilds vary across site?

#Objective:  Assign each bee species to their respective nesting guild

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = TRUE, na.strings = c("", "NA"))

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

#Years 1-3 ####
#-------------------------------------------------------------------#
#                             2014-2016                             #
#-------------------------------------------------------------------#

#Subset BeeIDs to include only 2014-2016
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016)

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
#write.csv(BeeIDs123byguildindwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbyAbundance123.csv")

#Create table showing the number of species in each guild by site and year
BeeIDs123byguildspecies <- BeeIDs123 %>%
  group_by(Site, Year, Guild) %>%
  summarise(NumberSpecies = length(unique(Binomial)))

#Reformat from long to wide format
BeeIDs123byguildspecieswide <- spread(BeeIDs123byguildspecies, Guild, NumberSpecies)

#Fill NAs with 0
BeeIDs123byguildspecieswide[is.na(BeeIDs123byguildspecieswide)] <- 0

#Export as .csv
#write.csv(BeeIDs123byguildspecieswide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySpecies123.csv")
  
#Create table showing number of individuals in each guild collected each year
BeeIDs123byguildyear <- BeeIDs123 %>%
  group_by(Year) %>%
  count(Guild)

#Change "Year" to a factor
BeeIDs123byguildyear$Year <- as.factor(BeeIDs123byguildyear$Year)

#Plot number of specimens collected in each guild by year
BeeIDs123byguildplot <- ggplot(BeeIDs123byguildyear,
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
BeeIDs123byguildbarplot <- ggplot(BeeIDs123byguildyear,
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
#write.csv(BeeIDs123byguildsite, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySite123.csv")

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

#Years 1-4 ####
#-------------------------------------------------------------------#
#                             2014-2017                             #
#-------------------------------------------------------------------#
#Subset BeeIDs to include only 2014-2017
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017)

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

#Check to make sure mutate function worked
BeeIDs1234 %>%
  group_by(Site, County) %>%
  summarise()

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
#Check to make sure RRW worked
BeeIDs1234 %>%
  group_by(Site, RRW) %>%
  summarise()
#Good!

#Create table showing the number of individuals within each guild by site and year
BeeIDs1234byguildind <- BeeIDs1234 %>%
  group_by(Site, Year, Guild) %>%
  count(Binomial)
BeeIDs1234byguildind <- BeeIDs1234byguildind %>%
  group_by(Site, Year, Guild) %>%
  summarise(Abundance = sum(n))

#Reformat from long to wide
BeeIDs1234byguildindwide <- spread(BeeIDs1234byguildind, Guild, Abundance)

#Fill NAs with 0
BeeIDs1234byguildindwide[is.na(BeeIDs1234byguildindwide)] <- 0

#Export as .csv
#write.csv(BeeIDs1234byguildindwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbyAbundance1234.csv")

#Create table showing the number of species in each guild by site and year
BeeIDs1234byguildspecies <- BeeIDs1234 %>%
  group_by(Site, Year, Guild) %>%
  summarise(NumberSpecies = length(unique(Binomial)))

#Reformat from long to wide format
BeeIDs1234byguildspecieswide <- spread(BeeIDs1234byguildspecies, Guild, NumberSpecies)

#Fill NAs with 0
BeeIDs1234byguildspecieswide[is.na(BeeIDs1234byguildspecieswide)] <- 0

#Export as .csv
#write.csv(BeeIDs1234byguildspecieswide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySpecies1234.csv")

#Create table showing the number of individuals within each guild by RRW
BeeIDs1234byguildRRW <- BeeIDs1234 %>%
  group_by(RRW, Guild) %>%
  count(Binomial)
BeeIDs1234byguildRRW <- BeeIDs1234byguildRRW %>%
  group_by(RRW, Guild) %>%
  summarise(Abundance = sum(n))

#Reformat from long to wide format
BeeIDs1234byguildRRWwide <- spread(BeeIDs1234byguildRRW, Guild, Abundance)

#Fill NAs with 0
BeeIDs1234byguildRRWwide[is.na(BeeIDs1234byguildRRWwide)] <- 0

#Export as .csv
#write.csv(BeeIDs1234byguildRRWwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbyRRW1234.csv")

#Create table showing number of individuals in each guild collected each year
BeeIDs1234byguildyear <- BeeIDs1234 %>%
  group_by(Year) %>%
  count(Guild)

#Change "Year" to a factor
BeeIDs1234byguildyear$Year <- as.factor(BeeIDs1234byguildyear$Year)

#Plot number of specimens collected in each guild by year
BeeIDs1234byguildplot <- ggplot(BeeIDs1234byguildyear,
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
BeeIDs1234byguildplot

#Do the same but in a grouped bar plot
BeeIDs1234byguildbarplot <- ggplot(BeeIDs1234byguildyear,
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
  scale_x_discrete(labels = wrap_format(20)) +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
BeeIDs1234byguildbarplot

#Create table showing number of individuals collected at each site during each year
BeeIDs1234byguildsite <- BeeIDs1234 %>%
  group_by(Year, Site) %>%
  count(Guild)
BeeIDs1234numberguilds <- BeeIDs1234 %>%
  group_by(Year, Site) %>%
  summarise(NumberGuilds = length(unique(Guild)))

#Export "BeeIDs123byguildsite" to .csv to use in SAS analyses
#write.csv(BeeIDs1234byguildsite, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySite1234.csv")

#Graph that shiz
BeeIDs1234byguildsiteplot <- ggplot(BeeIDs1234byguildsite,
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
BeeIDs1234byguildsiteplot

#Bar graph
BeeIDs1234byguildsitebarplot <- ggplot(BeeIDs1234byguildsite,
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
BeeIDs1234byguildsitebarplot

#Plot guild abundance by RRW
BeeIDs1234byguildRRWstack <- ggplot(BeeIDs1234byguildRRW) +
  geom_bar((aes(x = Guild,
              y = Abundance,
              fill = RRW)),
           stat = "identity",
           color = "black") +
  theme_bw() +
  labs(x = "",
       y = "Bee Abundance") +
  theme(legend.text = element_text(size = 10)) +
  guides(fill = guide_legend("Sites")) +
  theme(legend.title.align = 0.5) +
  theme(legend.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1)) +
  scale_x_discrete(labels = wrap_format(20)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62"))
BeeIDs1234byguildRRWstack

##Years 1-5 ####
#-------------------------------------------------------------------#
#                             2014-2018                             #
#-------------------------------------------------------------------#
#Calculate number of bees in each guild by site
bees.guild <- bees %>%
  group_by(Site, Guild) %>%
  count(Binomial)
bees.guild <- bees.guild %>%
  group_by(Site, Guild) %>%
  summarise(no.bees = sum(n))

#Reformat from long to wide
bees.guild.wide <- spread(bees.guild, Guild, no.bees)

#Fill NAs with 0
bees.guild.wide[is.na(bees.guild.wide)] <- 0

#Calculate number of bee species in each guild by site
beespp.guild <- bees %>%
  group_by(Site, Guild) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Reformat from long to wide
beespp.guild.wide <- spread(beespp.guild, Guild, no.beespp)

#Fill NAs with 0
beespp.guild.wide[is.na(beespp.guild.wide)] <- 0

#Create table showing the number of individuals within each guild by site and year
bees.guildsiteyear <- bees %>%
  group_by(Site, Year, Guild) %>%
  count(Binomial)
bees.guildsiteyear <- bees.guildsiteyear %>%
  group_by(Site, Year, Guild) %>%
  summarise(Abundance = sum(n))

#Reformat from long to wide
bees.guildsiteyear.wide <- spread(bees.guildsiteyear, Guild, Abundance)

#Fill NAs with 0
bees.guildsiteyear.wide[is.na(bees.guildsiteyear.wide)] <- 0

#Export as .csv
#write.csv(bees.guild.wide, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/Bee guilds by abundance.csv")

#Create table showing the number of species in each guild by site and year
beespp.guildyear <- bees %>%
  group_by(Site, Year, Guild) %>%
  summarise(no.beespp = length(unique(Binomial)))

#Reformat from long to wide format
beespp.guildyear.wide <- spread(beespp.guildyear, Guild, no.beespp)

#Fill NAs with 0
beespp.guildyear.wide[is.na(beespp.guildyear.wide)] <- 0

#Export as .csv
#write.csv(beespp.guild.wide, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/Bee guilds by species.csv")

#Create table showing number of individuals in each guild collected each year
bees.guildyear <- bees %>%
  group_by(Year) %>%
  count(Guild)

#Change "Year" to a factor
bees.guildyear$Year <- as.factor(bees.guildyear$Year)

#Plot number of specimens collected in each guild by year
bees.guildyear.plot <- ggplot(bees.guildyear,
                              aes(x = Guild,
                                  y = n)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 4) +
  theme_bw() + 
  labs(y = "Bee Abundance") +
  ggtitle("Bee Abundance in \nEach Guild by Year") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
bees.guildyear.plot

#Do the same but in a grouped bar plot
bees.guildyear.barplot <- ggplot(bees.guildyear,
                                 aes(x = Guild,
                                     y = n,
                                     fill = Year)) +
  geom_bar(color = "black",
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Bee Abundance in \nEach Guild by Year") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
bees.guildyear.barplot

#Create table showing number of individuals collected at each site during each year
bees.guildsite <- bees %>%
  group_by(Year, Site) %>%
  count(Guild)

#Export "BeeIDs123byguildsite" to .csv to use in SAS analyses
#write.csv(BeeIDs123byguildsite, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/BeeGuilds/GuildsbySite123.csv")

#Graph that shiz
bees.guildsite.plot <- ggplot(bees.guildsite,
                                aes(x = Guild,
                                    y = n)) +
  geom_point(aes(color = Site)) +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Bee Abundance in Each Guild by Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
bees.guildsite.plot

#Bar graph
bees.guildsite.barplot <- ggplot(bees.guildsite,
                                 aes(x = Guild,
                                     y = n,
                                     fill = Site)) +
  geom_bar(color = "black",
           position = "dodge",
           stat = "identity") +
  theme_bw() +
  labs(y = "Bee Abundance") +
  ggtitle("Bee Abundance in Each Guild by Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)  +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1))
bees.guildsite.barplot

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