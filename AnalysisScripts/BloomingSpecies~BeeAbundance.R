#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Abundance          #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and bee abundance
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(ggplot2)

#Read in data
nqAM <- read.csv("Moorhouse Full data set.csv")
#####Floral index in this data set DOES include weed species as well as forbs.
#####PercentCover in "New_Bees_Format.csv" is the average floral coverage for all 40 quadrats in year 1; all 50 in year 2.
#####For example: McClellan Year 1 (Site 9) had 0.3% coverage in all ten quadrats during the first sample of the year and nothing beyond that, so the average over all 4 samples is 0.3/4=0.075.
#####Quadrats in "New_Bees_Format.csv" is the proportion of quadrats over the entire year that contained blooming species.
#####For example: McClellan Year 1 (Site 9) had 1 quadrat during the entire year with anything blooming, so the proportion would be 1/40=0.025.
#####What does "Quadrats" column depict exactly? Total coverage? Average coverage?
#####Average coverage!
#####What is "Frequency of Blooming Species"?
#####Frequency = Percent Coverage

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listAM <- as.numeric(nqAM$Year)
pch.listAM

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqAM$Year <- as.factor(nqAM$Year)

#Amy's plot: Number Quadrats vs. Bee Abundance
plot(nqAM$Quadrats,nqAM$TotalAbundance,
     xlab="Frequency of Blooming Species",ylab="Bee Abundance",
     pch=c(pch.listAM),col='black')
modelAM=lm(nqAM$TotalAbundance~nqAM$Quadrats)
modelAM
summary(modelAM)
abline(modelAM)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelAM)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBAAM <- lm(TotalAbundance ~ Quadrats, data = nqAM)
summary(BSonBAAM)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBAAM)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BSonBAAMplot <- ggplot(nqAM, aes(x = Quadrats, y = TotalAbundance)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 54.003745, slope = 0.221903) +
  theme_bw() +
  labs(x = "Frequency of Blooming Species", y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBAAMplot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Abundance          #
#                             Year 3                                #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())

#Read in data
nqMMM <- read.csv("Mackert Full data set.csv")

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listMMM <- as.numeric(nqMMM$Year)
pch.listMMM

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqMMM$Year <- as.factor(nqMMM$Year)

#Amy's plot: Number Quadrats vs. Bee Abundance
plot(nqMMM$Quadrats,nqMMM$TotalAbundance,
     xlab="Frequency of Blooming Species",ylab="Bee Abundance",
     pch=c(pch.listMMM),col='black')
modelMMM=lm(nqMMM$TotalAbundance~nqMMM$Quadrats)
modelMMM
summary(modelMMM)
abline(modelMMM)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelMMM)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBAMMM <- lm(TotalAbundance ~ Quadrats, data = nqMMM)
summary(BSonBAMMM)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBAMMM)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BSonBAMMMplot <- ggplot(nqMMM, aes(x = Quadrats, y = TotalAbundance)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 36.577611, slope = 3.246725) +
  theme_bw() +
  labs(x = "Frequency of Blooming Species", y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBAMMMplot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Abundance          #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
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
Quadrats <- read.csv("Plants/Quadrats.csv")
#Date = Date of sample
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#Sample; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Site = Site name
#Quadrat = Quadrat number; 1-10
#Species = Name of plant(s) in quadrat
#X..Cover = Percent coverage of each species within quadrat
#X..Bare.Ground = Percent coverage of bare ground within quadrat
#Species.in.Strip...Not.in.Quadrats = Blooming plant species occurring within the study strip, but not detected within the quadrats
#Outside.Species = Blooming plant species occurring elsewhere on the property

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Only dealing with 2014-2016, so remove 2017
Data123 <- Fulldata %>%
  filter(Year <= 3)
Quadrats123 <- Quadrats %>%
  filter(Year <= 3)

#Year column is brought in as an integer. Change to factor for Morgan's plot.
Data123$Year <- as.factor(Data123$Year)

#Determine number of unique blooming species found in quadrats at each site during each year
bsquadrats123 <- Quadrats123 %>%
  group_by(Site) %>%
  summarise(TotalBS = length(unique(Species)))

#What are the blooming species?
bsquadrats123names <- Quadrats123 %>%
  group_by(Site) %>%
  count(Species)

#Determine average number of bees collected at each site during each year
bees123 <- Data123 %>%
  group_by(Site) %>%
  summarise(TotalBees = sum(Total.Abundance))

#Join all of the new data sets together
BSonBA123 <- full_join(bees123, bsquadrats123, by = c("Site"))

#Model for bee abundance predicted by frequency of blooming species
BSonBA123model <- lm(Total.Abundance ~ Blooming.Species, data = Data123)
summary(BSonBA123model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BSonBA123plot <- ggplot(BSonBA123,
                        aes(x = TotalBS,
                            y = TotalBees)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  #scale_color_manual(labels = c("2014", "2015", "2016"),
                     #values = c("darkorchid1", "#000000", "darkgreen")) +
  #scale_shape_manual(labels = c("2014", "2015", "2016"),
                     #values = c(16, 17, 15)) +
  labs(x = "Number of Blooming Plant Species",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Plant Species \non Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BSonBA123plot
