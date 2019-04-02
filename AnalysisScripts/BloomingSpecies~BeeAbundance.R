#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Abundance          #
#-------------------------------------------------------------------#

#Research Question: How does the number of blooming forb/weed species within the strip influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed species richness and bee abundance
#Use created model(s) to visualize the relationship graphically

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"))
Bees <- read.csv("Bees/Bee IDs.csv")

#Use lubridate to allow R to read the dates
Quadrats$Date <- mdy(Quadrats$Date)
Quadrats$Year <- year(Quadrats$Date)
Bees$Date <- mdy(Bees$Date)
Bees$Year <- year(Bees$Date)

#Fill NAs with 0 in Quadrats$Cover to indicate no plants were blooming at that point
Quadrats$Cover[is.na(Quadrats$Cover)] <- 0

#Determine number of plant species in bloom for each site/date
floral.species <- Quadrats %>%
  group_by(Site, Date) %>%
  summarise(no.floralspp = n_distinct(Species, na.rm = TRUE))

#Determine number of individuals of each species collected for each site/date
bees <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Date)) %>%
  count(Binomial) %>%
  summarise(total.bees = sum(n))

#Join floral.species and bees datasets together
floralspecies.bees <- full_join(bees, floral.species, by = c("Site", "Date"))

#Fill in 0 for any NAs in total.bees (showing we sampled vegetation, but collected no bees)
floralspecies.bees$total.bees[is.na(floralspecies.bees$total.bees)] <- 0

#Include Year column in floralcover.bees using lubridate
floralspecies.bees$Year <- year(floralspecies.bees$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#                             Years 1-2                             #
#-------------------------------------------------------------------#
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

#Year 3
#-------------------------------------------------------------------#
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

#Years 1-3 ####
#-------------------------------------------------------------------#
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

#Use lubridate to allow R to recognize the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Add new column with only the year
Fulldata$Year <- year(Fulldata$Date)

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset Fulldata to include only 2014-2016 data
years123 <- Fulldata %>%
  filter(Year <= 2016)

#Model for bee abundance predicted by frequency of blooming species
BSonBA123model <- lmer(Total.Abundance ~ Blooming.Species + (1|Site) + (1|Sampling.Period) + (1|Year),
                       data = years123)
summary(BSonBA123model)

#Null model not including floral cover
BSonBA123null <- lmer(Total.Abundance ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                      data = years123)
summary(BSonBA123null)

#Likelihood ratio test between the full and null models
anova(BSonBA123null, BSonBA123model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonBA123model)

#Year column is brought in as an integer. Change to factor for Morgan's plot.
years123$Year <- as.factor(years123$Year)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BSonBA123plot <- ggplot(years123,
                        aes(x = Blooming.Species,
                            y = Total.Abundance)) +
  geom_point(aes(color = Year,
                 shape = Year),
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
  labs(x = "Number of Blooming Forb Species",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb Species \non Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BSonBA123plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Models for bee abundance predicted by blooming plant species richness
BSonBA12345model <- lmer(total.bees ~ no.floralspp + (1|Site) * (1|Year),
                         data = floralspecies.bees)
summary(BSonBA12345model)
#AIC = 2393.772; p-value = 0.318666

BSonBA12345model2 <- lmer(total.bees ~ no.floralspp + (1|Year),
                          data = floralspecies.bees)
summary(BSonBA12345model2)
#AIC = 2397.076; p-value = 0.00224

BSonBA12345model3 <- lmer(total.bees ~ no.floralspp + (1|Site) + (1|Year),
                          data = floralspecies.bees)
summary(BSonBA12345model3)
#AIC = 2393.772; p-value = 0.318666

BSonBA12345model4 <- lmer(total.bees ~ no.floralspp + (1|Site) + (1|Year) + (1|Date),
                          data = floralspecies.bees)
summary(BSonBA12345model4)
#AIC = 2392.057; p-value = 0.142417
#Model 4 has lowest AIC value! Use this one.

BSonBA12345model5 <- lmer(total.bees ~ no.floralspp + (1|Site) * (1|Year) * (1|Date),
                          data = floralspecies.bees)
summary(BSonBA12345model5)
#AIC = 2392.057; p-value = 0.142417

BSonBA12345model6 <- lmer(total.bees ~ no.floralspp + (1|Date) * (1|Site),
                          data = floralspecies.bees)
summary(BSonBA12345model6)
#AIC = 2398.563; p-value = 0.0311

BSonBA12345model7 <- lmer(total.bees ~ no.floralspp + Date + (1|Site),
                          data = floralspecies.bees)
summary(BSonBA12345model7)
#AIC = 2402.298; p-value = 0.42717

#Check residuals
qqnorm(resid(BSonBA12345model4))
qqline(resid(BSonBA12345model4))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonBA12345model4)
#R2m = 0.01186844; R2c = 0.5205423

#Convert year to factor
floralspecies.bees$Year <- as.factor(floralspecies.bees$Year)

#Graph that shiz
BSonBA12345plot <- ggplot(floralspecies.bees,
                          aes(x = no.floralspp,
                              y = total.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F", "cornflowerblue")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c(15, 1, 17, 18, 25)) +
  theme_bw() +
  labs(x = "Blooming Forb Species Richness",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Species Richness \non Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBA12345plot
