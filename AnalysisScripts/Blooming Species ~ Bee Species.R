#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-2                             #
#-------------------------------------------------------------------#
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Read in data
nqAM <- read.csv("Moorhouse Full data set.csv")
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
#Clear environment and set working directory
rm(list=ls())

#Read in data
nqMMM <- read.csv("Mackert Full data set.csv")

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
#Clear environment and set working directory
rm(list=ls())

#Read in data
nqfull <- read.csv("Combined Full data set.csv")

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(nqfull$Year)
pch.listfull

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nqfull$Year <- as.factor(nqfull$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(nqfull$SppBloomQ,nqfull$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listfull),col='black')
modelfull=lm(nqfull$SppBloomQ~nqfull$Total.Species.Richness)
modelfull
summary(modelfull)
abline(modelfull)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(modelfull)$adj.r.squared,digits=4)))

#Model for bee species richness predicted by number of blooming species
BSonBSfull <- lm(Total.Species.Richness ~ SppBloomQ, data = nqfull)
summary(BSonBSfull)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBSfull)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBSfullplot <- ggplot(nqfull, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = XXX, slope = XXX) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBSfullplot
