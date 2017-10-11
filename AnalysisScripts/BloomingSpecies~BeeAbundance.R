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
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
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
nqfull <- read.csv("Combined Full data set.csv")

#Year column in "nqfull" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list<-as.numeric(nqfull$Year)
pch.list

#Year column in "nqfull" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqfull$Year <- as.factor(nqfull$Year)

#Amy's plot: Number Quadrats vs. Bee Abundance
plot(nqfull$Quadrats,nqfull$TotalAbundance,
     xlab="Frequency of Blooming Species",ylab="Bee Abundance",
     pch=c(pch.list),col='black')
model=lm(nqfull$TotalAbundance~nqfull$Quadrats)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBAfull <- lm(TotalAbundance ~ Quadrats, data = nqfull)
summary(BSonBAfull)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBAfull)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BSonBAfullplot <- ggplot(nqfull, aes(x = SppBloomQ, y = TotalAbundance)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 45.239054, slope = 1.801929) +
  theme_bw() +
  labs(x = "Frequency of Blooming Species", y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBAfullplot
