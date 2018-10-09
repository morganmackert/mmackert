#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#-------------------------------------------------------------------#

#Research Question: How does blooming forb/weed abundance influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed abundance and bee abundance
#Use created model(s) to visualize the relationship graphically

#Start ####

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
Fulldata <- read.csv("Combined Full data set.csv")
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
bees <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  count(Binomial) %>%
  summarise(total.bees = sum(n))

#Determine average floral cover for each site/date
floral.cover <- Quadrats %>%
  group_by(Site, Date, Quadrat) %>%
  summarise(floral.cover = sum(Cover))
floral.cover <- floral.cover %>%
  group_by(Site, Date) %>%
  summarise(avg.floralcover = mean(floral.cover))

#Join SpecRichAbund and FloralCover datasets together
floralcover.bees <- full_join(bees, floral.cover, by = c("Site", "Date"))

#Fill in 0 for any NAs in Total.Abundance (showing we sampled vegetation, but collected no bees)
floralcover.bees$total.bees[is.na(floralcover.bees$total.bees)] <- 0

#Include Year column in floralcover.bees using lubridate
floralcover.bees$Year <- year(floralcover.bees$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                           Years 1-2                               #
#-------------------------------------------------------------------#
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

#Years 1-3 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                           Years 1-3                               #
#-------------------------------------------------------------------#
#Subset BAonBA to include only 2014-2016 data.
BAonBA123 <- filter(BAonBA, Year <= 3)

#Year column brought in as an integer; change to factor.
BAonBA123$Year <- as.factor(BAonBA123$Year)

#Model for bee abundance predicted by frequency of blooming species
BAonBA123model <- lm(BeeAbundance ~ AverageFloralCover,
                     data = BAonBA123)
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
  scale_color_manual(labels = c("2014", "2015", "2016"),
                     values = c("darkorchid1", "#000000", "darkgreen")) +
  scale_shape_manual(labels = c("2014", "2015", "2016"),
                     values = c(16, 17, 15)) +
  theme_bw() +
  labs(x = "Average Blooming Plant Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BAonBA123plot

#Years 3-4 ####
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

#Year 4 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                             Year 4                                #
#-------------------------------------------------------------------#
#Subset BAonBA to include only 2016-2017 data.
BAonBA4 <- filter(BAonBA, Year == 4)

#Year column brought in as an integer; change to factor.
BAonBA4$Year <- as.factor(BAonBA4$Year)

#Model for bee abundance predicted by blooming plant coverage
BAonBA4model <- glmer(BeeAbundance ~ AverageFloralCover + (1|Sampling.Period) + (1|Site),
                      data = BAonBA4,
                      family = "poisson")
summary(BAonBA4model)

#Null model not including average floral cover
BAonBA4null <- glmer(BeeAbundance ~ (1|Sampling.Period) + (1|Site),
                     data = BAonBA4,
                     family = "poisson")
summary(BAonBA4null)

#Likelihood ratio test between null and full models
anova(BAonBA4null, BAonBA4model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BAonBA4model, residuals(BAonBA4model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonBA4model)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
BAonBA4plot <- ggplot(BAonBA4, aes(x = AverageFloralCover,
                                   y = BeeAbundance)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance")
BAonBA4plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#           Blooming Forb and Weed Abundance ~ Bee Abundance        #
#                             Years 1-5                             #
#-------------------------------------------------------------------#

#Model for bee abundance predicted by blooming plant coverage
BAonBA12345model <- lmer(total.bees ~ avg.floralcover + (1|Site) * (1|Year),
                        data = floralcover.bees)
summary(BAonBA12345model)
anova(BAonBA12345model)

#Check residuals
qqnorm(resid(BAonBA12345model))
qqline(resid(BAonBA12345model))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonBA12345model)

#Convert year to factor
floralcover.bees$Year <- as.factor(floralcover.bees$Year)

#Graph that shiz
BAonBA12345plot <- ggplot(floralcover.bees,
                          aes(x = avg.floralcover,
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
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA12345plot

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

#Change names so they're not so goofy
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Floral.Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Species"

names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"

#Determine average floral cover for each site (for data table)
AverageFloralCover <- Quadrats %>%
  group_by(Site, Date, Quadrat) %>%
  summarise(TotalFloralCover = sum(Floral.Cover))
AverageFloralCover <- AverageFloralCover %>%
  group_by(Site) %>%
  summarise(AverageFloralCover = mean(TotalFloralCover))

#Calculate average floral cover and number of bees collected via emergence traps at each site during each year.
BAonBA <- Fulldata %>%
  group_by(Year, Site, Sampling.Period) %>%
  summarise(AverageFloralCover = mean(Floral.Cover),
            BeeAbundance = sum(Total.Abundance))

#Null model not including average floral cover
BAonBA1234null <- lmer(BeeAbundance ~ (1|Sampling.Period) + (1|Site) + (1|Year),
                       data = BAonBA1234)
summary(BAonBA1234null)

#Likelihood ratio test between null and full models
anova(BAonBA1234null, BAonBA1234model)