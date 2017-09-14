#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence bee species richness?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and bee species richness
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(ggplot2)

#Read in data
nqAM <- read.csv("mmackert/Data/Moorhouse Full data set.csv")
#####Floral index in this data set DOES include weed species as well as forbs.
#####PercentCover in "New_Bees_Format.csv" is the average floral coverage for all 40 quadrats in year 1; all 50 in year 2.
#####For example: McClellan Year 1 (Site 9) had 0.3% coverage in all ten quadrats during the first sample of the year and nothing beyond that, so the average over all 4 samples is 0.3/4=0.075.
#####Quadrats in "New_Bees_Format.csv" is the proportion of quadrats over the entire year that contained blooming species.
#####For example: McClellan Year 1 (Site 9) had 1 quadrat during the entire year with anything blooming, so the proportion would be 1/40=0.025.

#Date = Date of sample
#Site = Site name
#Sampling = Sample period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016
#Quadrats = Combined coverage of blooming forb/weed species in ten quadrats
#SppBloomQ = Number of forb/weed species in bloom within ten quadrats
#BareGround = Average bare ground coverage in ten quadrats
#TotalAbundance = Total number of bees collected
#Total.Genus.Richness = Total number of bee genera collected
#Total.Species.Richness = Total number of bee species collected
#Following species names correspond to number of individuals collected of that species

##### REMOVE ONE OF EARLY MAY NEAL SMITH SAMPLES??? #####

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listAM <- as.numeric(nqAM$Year)
pch.listAM

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqAM$Year <- as.factor(nqAM$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(nqAM$SppBloomQ,nqAM$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listAM),col='black')
modelAM=lm(nqAM$SppBloomQ~nqAM$Total.Species.Richness)
modelAM
summary(modelAM)
abline(modelAM)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelAM)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBSAM <- lm(Total.Species.Richness ~ SppBloomQ, data = nqAM)
summary(BSonBSAM)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBSAM)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBSAMplot <- ggplot(nqAM, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 8.406818, slope = 1.071947) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBSAMplot

##### COMING SOON #####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Year 3                                #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
nqMMM <- read.csv("mmackert/Data/Mackert Full data set.csv")

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listMMM <- as.numeric(nqMMM$Year)
pch.listMMM

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqMMM$Year <- as.factor(nqMMM$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(nqMMM$SppBloomQ,nqMMM$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listMMM),col='black')
modelMMM=lm(nqMMM$SppBloomQ~nqMMM$Total.Species.Richness)
modelMMM
summary(modelMMM)
abline(modelMMM)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelMMM)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBSMMM <- lm(Total.Species.Richness ~ SppBloomQ, data = nqMMM)
summary(BSonBSMMM)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBSMMM)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBSMMMplot <- ggplot(nqMMM, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = XXX, slope = XXX) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBSMMMplot

##### COMING SOON #####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
years123 <- read.csv("mmackert/Data/Combined Full data set.csv")

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(years123$Year)
pch.listfull

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
years123$Year <- as.factor(years123$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(years123$SppBloomQ,nqfull$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listfull),col='black')
modelfull=lm(nqfull$SppBloomQ~nqfull$Total.Species.Richness)
modelfull
summary(modelfull)
abline(modelfull)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelfull)$adj.r.squared,digits=4)))

#Model for bee species richness predicted by number of blooming species
BSonBSfull <- lm(Total.Species.Richness ~ SppBloomQ, data = years123)
summary(BSonBSfull)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBSfull)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBSfullplot <- ggplot(years123, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = -0.8941012, slope = 4.8657711) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBSfullplot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 3-4                             #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Read in data
years34 <- read.csv("Data/Condensed34.csv")

#Year column in "years34" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(years34$Year)
pch.listfull

#Year column in "years34" dataframe is brought in as an integer. Change to factor for Morgan's plot.
years34$Year <- as.factor(years34$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(years34$SppBloomQ,years34$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listfull),col='black')
modelfull=lm(years34$SppBloomQ~years34$Total.Species.Richness)
modelfull
summary(modelfull)
abline(modelfull)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelfull)$adj.r.squared,digits=4)))

#Model for bee species richness predicted by number of blooming species
BSonBS34 <- lm(TotalSpeciesRichness ~ SppBloomQ, data = years34)
summary(BSonBS34)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBS34)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS34plot <- ggplot(years34, aes(x = SppBloomQ, y = TotalSpeciesRichness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 16.492068, slope = 1.484885) +
  #annotate("text", x = 4, y = 41, label = "R^2 = 0.8286") +
  #annotate("text", x = 5, y = 45, label = "y = 1.48x + 16.49") +
  scale_color_hue(labels = c("2016", "2017")) +
  scale_shape_manual(labels = c("2016", "2017"), values = c(16, 17)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS34plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Read in data
years1234 <- read.csv("Data/Combined full dataset condensed.csv")

#Year column in "years1234" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(years1234$Year)
pch.listfull

#Year column in "years1234" dataframe is brought in as an integer. Change to factor for Morgan's plot.
years1234$Year <- as.factor(years1234$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(years1234$SppBloomQ,years1234$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listfull),col='black')
modelfull=lm(years1234$SppBloomQ~years1234$Total.Species.Richness)
modelfull
summary(modelfull)
abline(modelfull)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelfull)$adj.r.squared,digits=4)))

#Model for bee species richness predicted by number of blooming species
BSonBS1234 <- lm(TotalSpeciesRichness ~ SppBloomQ, data = years1234)
summary(BSonBS1234)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBS1234)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS1234plot <- ggplot(years1234, aes(x = SppBloomQ, y = TotalSpeciesRichness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 18.841286, slope = 1.366183) +
  #annotate("text", x = 4, y = 41, label = "R^2 = 0.8286") +
  #annotate("text", x = 5, y = 45, label = "y = 1.48x + 16.49") +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"), values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"), values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Number of Plant Species in Bloom", y = "Number of Bee Species") +
  ggtitle("Influence of the Number of Blooming Plant \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BSonBS1234plot
