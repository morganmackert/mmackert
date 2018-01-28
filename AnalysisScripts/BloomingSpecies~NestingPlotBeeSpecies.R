#-------------------------------------------------------------------#
#     Blooming Forb and Weed Species ~ Nesting Plot Bee Species     #
#                              Year 4                               #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(lme4)
library(MuMIn)
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
Fulldata <- read.csv("Combined full data set.csv")
#Date = Date of sample
#Site = Site name
#Sampling.Period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#X..Floral.Cover..in.10m2. = Average coverage of blooming forb/weed species in ten quadrats
#X..Blooming.species.in.quadrats = Number of forb/weed species in bloom within ten quadrats
#X..Bare.Ground..in.10m2. = Average bare ground coverage in ten quadrats
#Trapname.Abundance = Number of individual bees collected by specified trap/site/date
#Total.Abundance = Number of individual bees collected by all trap types at the specified site/date
#Trapname.Species.Richness = Number of bee species collected by specified trap/site/date
#Total.Species.Richness = Number of bee species collected by all trap types at the specified site/date
#Species.Name = Number of individuals of specified species collected at the specified site/date

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)
Fulldata$Date <- mdy(Fulldata$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)
Fulldata$Year <- year(Fulldata$Date)

#Change column names so they're not so goofy
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset only 2017 nesting plot bees, not including wasps or unidentifiable specimens
BeeIDs4NP <- BeeIDs %>%
  filter(Year == 2017) %>%
  filter(Trap == "Plot") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")

#Subset Fulldata to include only 2017
year4 <- Fulldata %>%
  filter(Year == 2017)

#Table describing the number of individuals per species at each site and date
BeeIDs4NPbysitedate <- BeeIDs4NP %>%
  group_by(Site, Date) %>%
  count(Binomial)

#Table describing the number of individuals per species at each site
BeeIDs4NPbysite <- BeeIDs4NP %>%
  group_by(Site) %>%
  count(Binomial)

#Table describing the number of species collected at each site and date
BeeIDs4NPsppbysitedate <- BeeIDs4NP %>%
  group_by(Site, Date) %>%
  summarise(Total.Richness = length(unique(Binomial)))

#Table describing the number of species collected at each site
BeeIDs4NPsppbysite <- BeeIDs4NP %>%
  group_by(Site) %>%
  summarise(Total.Richness = length(unique(Binomial)))

#Convert "Year" to a  factor
year4$Year <- as.factor(year4$Year)

#Model for bee species richness predicted by number of blooming species
BSonNPBS4model <- lmer(Nesting.Plot.Species.Richness ~ Blooming.Species + (1|Site) + (1|Sampling.Period),
                        data = year4)
summary(BSonNPBS4model)

#Null model not including number of blooming species
BSonNPBS4null <- lmer(Nesting.Plot.Species.Richness ~ (1|Site) + (1|Sampling.Period),
                       data = year4)
summary(BSonNPBS4null)

#Likelihood ratio test between the full and null models
anova(BSonNPBS4null, BSonNPBS4model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BSonNPBS4model, residuals(BSonNPBS4model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonNPBS4model)

#Plot of the number of blooming forb/weed species vs. nesting plot bee species richness
BSonNPBS4plot <- ggplot(year4, 
                         aes(x = Blooming.Species,
                             y = Nesting.Plot.Species.Richness)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom",
       y = "Number of Nesting Plot Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Nesting Plot Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonNPBS4plot
