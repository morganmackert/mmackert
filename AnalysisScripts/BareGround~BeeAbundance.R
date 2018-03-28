#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 1-2                             #
#-------------------------------------------------------------------#

#Research Question: How does the presence/absence of bare ground within the strips influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between bare ground abundance and bee abundance
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(ggplot2)
library(dplyr)
library(lme4)

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

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Use lubridate to allow R to read the dates
Fulldata$Date <- mdy(Fulldata$Date)

#Calculate average bare ground and number of bees collected via emergence traps at each site during each sampling event.
BGonBA <- Fulldata %>%
  group_by(Date, Site, Year, Sampling.Period) %>%
  summarise(AverageBareGround = mean(Bare.Ground),
            ETrapAbundance = sum(Emergence.Traps.Abundance))

#Condense data by combining all sampling dates for each site
BGonBAcondensed <- Fulldata %>%
  group_by(Site, Year) %>%
  summarise(AverageBareGround = mean(Bare.Ground),
            ETrapAbundance = sum(Emergence.Traps.Abundance))

#Subset BGonBA to include only 2014 and 2015 data.
BGonBA12 <- filter(BGonBA, Year <= 2)

#Year column is brought in as an integer. Change to numeric for Amy's plot.
pch.list12 <- as.numeric(BGonBA12$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(BGonBA12$AverageBareGround, BGonBA12$ETrapAbundance,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = (pch.list12), col = "black")
model=lm(BGonBA12$ETrapAbundance~BGonBA12$AverageBareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(BGonBA12$ETrapAbundance~0+BGonBA12$AverageBareGround)
summary(model2)
abline(model2, lty="dotted")

#Model for bee abundance predicted by bare ground
BGYSonBA12model <- glm(ETrapAbundance ~ AverageBareGround + Year + Site,
                     family = poisson,
                     data = BGonBA12)
summary(BGYSonBA12model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonBA12model <- glm(ETrapAbundance ~ AverageBareGround,
                      data = BGonBA12)
summary(BGonBA12model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA12model)

#Change "Year" column to a factor.
BGonBA12$Year <- as.factor(BGonBA12$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA12plot <- ggplot(BGonBA12, aes(x = AverageBareGround,
                                     y = ETrapAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA12plot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                            Years 1-3                              #
#-------------------------------------------------------------------#
#Subset data frames to include only 2014-2016 data.
BGonBA123 <- filter(BGonBA, Year <= 3)
BGonBA123condensed <- filter(BGonBAcondensed, Year <= 3)

#Model for bee abundance predicted by bare ground including Year and Site as a random effects.
BGYSonBA123model <- glmer(ETrapAbundance ~ AverageBareGround + (1|Date) + (1|Site),
                          family = poisson,
                          data = BGonBA123)
summary(BGYSonBA123model)
coef(BGYSonBA123model)

#Model for emergence trap bee abundance predicted by bare ground using Year and Site as random effects.
BGYSonBA123condensedmodel <- glmer(ETrapAbundance ~ AverageBareGround + (1|Year) + (1|Site),
                                   family = poisson,
                                   data = BGonBA123condensed)
summary(BGYSonBA123condensedmodel)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA123model)

#Change "Year" column to factor.
BGonBA123$Year <- as.factor(BGonBA123$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA123plot <- ggplot(BGonBA123, aes(x = AverageBareGround,
                                       y = ETrapAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA123plot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 3-4                             #
#-------------------------------------------------------------------#
#Subset BGonBA to include only 2016-2017 data.
BGonBA34 <- filter(BGonBA, Year >= 3)

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGYSonBA34model <- glm(ETrapAbundance ~ AverageBareGround + Year + Site,
                     family = poisson,
                     data = BGonBA34)
summary(BGYSonBA34model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonBA34model <- glm(ETrapAbundance ~ AverageBareGround,
                     data = BGonBA34)
summary(BGonBA34model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA34model)

#Change "Year" column to factor.
BGonBA34$Year <- as.factor(BGonBA34$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA34plot <- ggplot(BGonBA34, aes(x = AverageBareGround,
                                     y = ETrapAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA34plot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                              Year 4                               #
#-------------------------------------------------------------------#
#Subset BGonBA to include only 2017 data.
BGonBA4 <- filter(BGonBA, Year == 2017)

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonBA4model <- glmer(ETrapAbundance ~ AverageBareGround + (1|Sampling.Period) + (1|Site),
                      data = BGonBA4,
                      family = "poisson")
summary(BGonBA4model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonBA4null <- glmer(ETrapAbundance ~ (1|Sampling.Period) + (1|Site),
                     data = BGonBA4,
                     family = "poisson")
summary(BGonBA4null)

#Likelihood ratio test between the full and null models
anova(BGonBA4null, BGonBA4model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBA4model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA4model)

#Change "Year" column to factor.
BGonBA4$Year <- as.factor(BGonBA4$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA4plot <- ggplot(BGonBA4, aes(x = AverageBareGround,
                                   y = ETrapAbundance)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA4plot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Subset BGonBA to include only 2014-2017 data.
BGonBA1234 <- filter(BGonBA, Year <= 4)

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonBA1234model <- lmer(ETrapAbundance ~ AverageBareGround + (1|Year) + (1|Site),
                         data = BGonBA1234)
summary(BGonBA1234model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonBA1234null <- glm(ETrapAbundance ~ (1|Year) + (1|Site),
                       family = poisson,
                       data = BGonBA1234)
summary(BGonBA1234null)
#Doesn't converge ugh

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBA1234model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA1234model)

#Change "Year" column to factor.
BGonBA1234$Year <- as.factor(BGonBA1234$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA1234plot <- ggplot(BGonBA1234, aes(x = AverageBareGround,
                                     y = ETrapAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA1234plot