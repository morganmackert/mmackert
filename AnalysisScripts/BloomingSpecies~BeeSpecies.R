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
BSonBS12 <- read.csv("mmackert/Data/Moorhouse Full data set.csv")

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

#Floral index in this data set DOES include weed species as well as forbs.
#PercentCover in "New_Bees_Format.csv" is the average floral coverage for all 40 quadrats in year 1; all 50 in year 2.
#For example: McClellan Year 1 (Site 9) had 0.3% coverage in all ten quadrats during the first sample of the year and nothing beyond that, so the average over all 4 samples is 0.3/4=0.075.
#Quadrats in "New_Bees_Format.csv" is the proportion of quadrats over the entire year that contained blooming species.
#For example: McClellan Year 1 (Site 9) had 1 quadrat during the entire year with anything blooming, so the proportion would be 1/40=0.025.

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list12 <- as.numeric(BSonBS12$Year)
pch.list12

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
BSonBS12$Year <- as.factor(BSonBS12$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(BSonBS12$SppBloomQ,BSonBS12$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listAM),col='black')
model12=lm(BSonBS12$SppBloomQ~nqAM$Total.Species.Richness)
model12
summary(model12)
abline(model12)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model12)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBS12model <- lm(Total.Species.Richness ~ SppBloomQ, data = BSonBS12)
summary(BSonBS12model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBS12model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS12plot <- ggplot(BSonBS12, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 8.406818, slope = 1.071947) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBS12plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Year 3                                #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
BSonBS3 <- read.csv("mmackert/Data/Condensed3.csv")

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list3 <- as.numeric(BSonBS3$Year)
pch.list3

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
BSonBS3$Year <- as.factor(BSonBS3$Year)

#Amy's plot: Blooming species richness vs. Bee species richness
plot(BSonBS3$SppBloomQ,BSonBS3$Total.Species.Richness,
     xlab="Number of Blooming Species",ylab="Number of Bee Species",
     pch=c(pch.listMMM),col='black')
model3=lm(BSonBS3$SppBloomQ~BSonBS3$Total.Species.Richness)
model3
summary(model3)
abline(model3)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model3)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBS3model <- lm(Total.Species.Richness ~ SppBloomQ, data = BSonBS3)
summary(BSonBS3model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBS3model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS3plot <- ggplot(BSonBS3, aes(x = SppBloomQ, y = Total.Species.Richness)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = XXX, slope = XXX) +
  theme_bw() +
  labs(x = "Number of Blooming Species", y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBS3plot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Species            #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

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

#Use lubridate to allow R to recognize the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset only years 1-2
Data123 <- filter(Fulldata, Year <= 3)

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
Data123$Year <- as.factor(Data123$Year)

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
BSonBS123model <- lm(Total.Species.Richness ~ Blooming.Species, data = Data123)
summary(BSonBS123model)

#Find intercept and slope to plot best fit line on graph; insert these values in the "geom_abline" line of the graph code
coef(BSonBS123model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee species richness
BSonBS123plot <- ggplot(Data123,
                        aes(x = Blooming.Species,
                            y = Total.Species.Richness)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  scale_color_manual(labels = c("2014", "2015", "2016"),
                     values = c("darkorchid1", "#000000", "darkgreen")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"),
                     values = c(16, 17, 15)) +
  labs(x = "Number of Blooming Species",
       y = "Number of Bee Species") +
  #ggtitle("Influence of Blooming Forb and Weed \nSpecies on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BSonBS123plot

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
