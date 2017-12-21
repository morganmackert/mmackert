#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How does blooming forb/weed abundance influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed abundance and bee abundance
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(ggplot2)

#Read in data
Fulldata <- read.csv("Combined Full data set.csv")
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

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Calculate average floral cover and number of bees collected via emergence traps at each site during each year.
BAonBA <- Fulldata %>%
  select(Site, Year, Floral.Cover, Total.Abundance) %>%
  group_by(Year, Site) %>%
  summarise(AverageFloralCover = mean(Floral.Cover),
            BeeAbundance = sum(Total.Abundance))

#Subset BAonBA to include only 2014 and 2015 data.
BAonBA12 <- filter(BAonBA, Year <= 2)

#Year column brought in as an integer. Change to numeric for Amy's plot.
pch.list12 <- as.numeric(BAonBA12$Year)
pch.list12

#Year column brought in as an integer. Change to factor for Morgan's plot.
BAonBA12$Year <- as.factor(BAonBA12$Year)

#Amy's plot: Number Quadrats vs. Bee Abundance
plot(BAonBA12$AverageFloralCover,BAonBA12$BeeAbundance,
     xlab="Frequency of Blooming Species",ylab="Bee Abundance",
     pch=c(pch.list12),col='black')
modelAM=lm(BAonBA12$BeeAbundance~BAonBA12$AverageFloralCover)
modelAM
summary(modelAM)
abline(modelAM)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelAM)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BAonBA12model <- lm(BeeAbundance ~ AverageFloralCover, data = BAonBA12)
summary(BAonBA12model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BAonBA12model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BAonBA12plot <- ggplot(BAonBA12, 
                       aes(x = AverageFloralCover,
                           y = BeeAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA12plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                           Years 1-3                               #
#-------------------------------------------------------------------#
#Subset BAonBA to include only 2014-2016 data.
BAonBA123 <- filter(BAonBA, Year <= 3)

#Year column brought in as an integer; change to factor.
BAonBA123$Year <- as.factor(BAonBA123$Year)

#Model for bee abundance predicted by frequency of blooming species
BAonBA123model <- lm(BeeAbundance ~ AverageFloralCover, data = BAonBA123)
summary(BAonBA123model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BAonBA123)

#Plot: Number of blooming forb/weed species vs. Bee Abundance
BAonBA123plot <- ggplot(BAonBA123, aes(x = AverageFloralCover,
                                       y = BeeAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Average Blooming Plant Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA123plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                             Years 3-4                             #
#-------------------------------------------------------------------#
#Subset BAonBA to include only 2016-2017 data.
BAonBA34 <- filter(BAonBA, Year >= 3)

#Year column brought in as an integer; change to factor.
BAonBA34$Year <- as.factor(BAonBA34$Year)

#Model for bee abundance predicted by frequency of blooming species
BAonBA34model <- lm(BeeAbundance ~ AverageFloralCover, data = BAonBA34)
summary(BAonBA34model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BAonBA34model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BAonBA34plot <- ggplot(BAonBA34, aes(x = AverageFloralCover,
                                     y = BeeAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA34plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Subset BAonBA to include only 2016-2017 data.
BAonBA1234 <- filter(BAonBA, Year <= 4)

#Year column brought in as an integer; change to factor.
BAonBA1234$Year <- as.factor(BAonBA1234$Year)

#Model for bee abundance predicted by frequency of blooming species
BAonBA1234model <- lm(BeeAbundance ~ AverageFloralCover, data = BAonBA1234)
summary(BAonBA1234model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BAonBA1234model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BAonBA1234plot <- ggplot(BAonBA1234, aes(x = AverageFloralCover,
                                         y = BeeAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA1234plot