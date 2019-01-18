#-------------------------------------------------------------------#
#       Blooming Forb and Weed Abundance ~ Bee Species Richness     #
#-------------------------------------------------------------------#

#Research Question: How does blooming forb/weed abundance influence bee species richness?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed abundance and bee species richness
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
Bees <- read.csv("Bees/Bee IDs.csv")
Quadrats <- read.csv("Plants/Quadrats.csv")

#Format date with lubridate
Bees$Date <- mdy(Bees$Date)
Bees$Year <- year(Bees$Date)
Quadrats$Date <- mdy(Quadrats$Date)
Quadrats$Year <- year(Quadrats$Date)

#Fill NAs with 0 in Quadrats$Floral.Cover to indicate no plants were blooming at that point
Quadrats$Cover[is.na(Quadrats$Cover)] <- 0

#Determine number of individuals of each species collected for each site/date
bees.spp <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Date)) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Determine average floral cover for each site/date
floral.cover <- Quadrats %>%
  group_by(Site, Date, Quadrat) %>%
  summarise(floral.cover = sum(Cover))
floral.cover <- floral.cover %>%
  group_by(Site, Date) %>%
  summarise(avg.floralcover = mean(floral.cover))

#Join floral.cover and bees.spp datasets together
floralcover.beespp <- full_join(bees.spp, floral.cover, by = c("Site", "Date"))

#Fill in 0 for any NAs in no.beespp (showing we sampled vegetation, but collected no bees)
floralcover.beespp$no.beespp[is.na(floralcover.beespp$no.beespp)] <- 0

#Include Year column in floralcover.beespp using lubridate
floralcover.beespp$Year <- year(floralcover.beespp$Date)

#Years 1-3 ####
#-------------------------------------------------------------------#
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only 2014-2016 data
years123 <- Fulldata %>%
  filter(Year <= 2016)

#Year column brought in as an integer. Change to factor for Morgan's plot.
years123$Year <- as.factor(years123$Year)

#Model for bee abundance predicted by frequency of blooming species
BAonBS123model <- lmer(Total.Species.Richness ~ Floral.Cover + (1|Site) + (1|Sampling.Period) + (1|Year),
                       data = years123)
summary(BAonBS123model)

#Null model not including floral cover
BAonBS123null <- lmer(Total.Species.Richness ~ (1|Site) + (1|Sampling.Period) + (1|Year),
                      data = years123)
summary(BAonBS123null)

#Likelihood ratio test between the full and null models
anova(BAonBS123null, BAonBS123model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonBS123model)

#Use ggResidpanel to view residual plots
resid_panel(resid(BAonBS123model), fitted(BAonBS123model))

#Convert Year to a factor
years123$Year <- as.factor(years123$Year)

#Graph average blooming cover vs. bee species richness
BAonBS123plot <- ggplot(years123,
                        aes(x = Floral.Cover,
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
  labs(x = "Blooming Forb Coverage (%)",
       y = "Number of Bee Species") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BAonBS123plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Models for bee abundance predicted by blooming plant coverage
BAonBS12345model <- lmer(no.beespp ~ avg.floralcover + (1|Site) * (1|Year),
                         data = floralcover.beespp)
summary(BAonBS12345model)
#AIC = 1295.97; p-value < 0.001

BAonBS12345model2 <- lmer(no.beespp ~ avg.floralcover + (1|Year),
                          data = floralcover.beespp)
summary(BAonBS12345model2)
#AIC = 1349.096; p-value < 0.001

BAonBS12345model3 <- lmer(no.beespp ~ avg.floralcover + (1|Site) + (1|Year),
                          data = floralcover.beespp)
summary(BAonBS12345model3)
#AIC = 1295.97; p-value < 0.001

BAonBS12345model4 <- lmer(no.beespp ~ avg.floralcover + (1|Site) + (1|Year) + (1|Date),
                          data = floralcover.beespp)
summary(BAonBS12345model4)
#AIC = 1290.943; p-value < 0.001
#Model 4 has lowest AIC value! Use this one.

BAonBS12345model5 <- lmer(no.beespp ~ avg.floralcover + (1|Site) * (1|Year) * (1|Date),
                          data = floralcover.beespp)
summary(BAonBS12345model5)
#AIC = 1290.943; p-value < 0.001

BAonBS12345model6 <- lmer(no.beespp ~ avg.floralcover + (1|Date) * (1|Site),
                          data = floralcover.beespp)
summary(BAonBS12345model6)
#AIC = 1316.466; p-value < 0.001

BAonBS12345model7 <- lmer(no.beespp ~ avg.floralcover + Date + (1|Site),
                          data = floralcover.beespp)
summary(BAonBS12345model7)
#AIC = 1324.411; p-value < 0.001

#Check residuals
qqnorm(resid(BAonBS12345model4))
qqline(resid(BAonBS12345model4))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonBS12345model4)
#R2m = 0.08933267; R2c = 0.7583837

#Convert year to factor
floralcover.beespp$Year <- as.factor(floralcover.beespp$Year)

#Graph that shiz
BAonBS12345plot <- ggplot(floralcover.beespp,
                          aes(x = avg.floralcover,
                              y = no.beespp)) +
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
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Species Richness") +
  ggtitle("Influence of Blooming Forb \nCoverage on Bee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BAonBS12345plot

#Data dictionary ####
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
#Old code ####
#Read in data
Fulldata <- read.csv("Combined Full data set.csv")

#Use lubridate to allow R to recognize the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Add new column with only the year
Fulldata$Year <- year(Fulldata$Date)

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"
