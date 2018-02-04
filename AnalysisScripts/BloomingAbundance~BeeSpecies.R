#-------------------------------------------------------------------#
#       Blooming Forb and Weed Abundance ~ Bee Species Richness     #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Research Question: How does blooming forb/weed abundance influence bee species richness?

#Objectives:
#Create model(s) to explore relationship between blooming forb/weed abundance and bee species richness
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(ggResidpanel)

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
#Subset BAonBA to include only 2014-2017 data.
BAonBA1234 <- filter(BAonBA, Year <= 4)

#Year column brought in as an integer; change to factor.
BAonBA1234$Year <- as.factor(BAonBA1234$Year)

#Model for bee abundance predicted by blooming plant coverage
BAonBA1234model <- lmer(BeeAbundance ~ AverageFloralCover + (1|Sampling.Period) + (1|Site) + (1|Year),
                        data = BAonBA1234)
summary(BAonBA1234model)

#Null model not including average floral cover
BAonBA1234null <- lmer(BeeAbundance ~ (1|Sampling.Period) + (1|Site) + (1|Year),
                       data = BAonBA1234)
summary(BAonBA1234null)

#Likelihood ratio test between null and full models
anova(BAonBA1234null, BAonBA1234model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BAonBA1234model, residuals(BAonBA1234model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonBA1234model)

#Graph that shiz
BAonBA1234plot <- ggplot(BAonBA1234,
                         aes(x = AverageFloralCover,
                             y = BeeAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Blooming Species Coverage (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb \nCoverage on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BAonBA1234plot
