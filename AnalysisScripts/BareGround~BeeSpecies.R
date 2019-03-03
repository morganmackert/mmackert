#-------------------------------------------------------------------#
#            Percent Bare Ground ~ Bee Species Richness             #
#-------------------------------------------------------------------#

#Research Question: How does the amount of bare ground present within the strips influence bee species richness?

#Objectives:
#Create model(s) to explore relationship between bare ground availability and bee species richness
#Use created model(s) to visualize the relationship graphically

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(lme4)
library(lmerTest)
library(MuMIn)

#Read in data
Quadrats <- read.csv("Plants/Quadrats.csv")
Bees <- read.csv("Bees/Bee IDs.csv")

#Use lubridate to allow R to read the dates
Quadrats$Date <- mdy(Quadrats$Date)
Quadrats$Year <- year(Quadrats$Date)
Bees$Date <- mdy(Bees$Date)
Bees$Year <- year(Bees$Date)

#Set BareGround column to numeric (must change to character first though)
Quadrats$BareGround <- as.numeric(as.character(Quadrats$BareGround))

#Calculate total bare ground
bareground <- Quadrats %>%
  filter(!is.na(BareGround)) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average bare ground cover for each site and date
avg.bareground <- bareground %>%
  group_by(Date, Site) %>%
  summarise(avg.bareground = mean(total.bareground), 
            number.quadrats = length(total.bareground))
#Two entries have only 9 quadrats included in calculation due to absences in the original 2014 data set

#Calculate number of bees collected via all traps
bee.spp <- Bees %>%
  group_by(Site, Date) %>%
  filter(!is.na(Date)) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Wasp") %>%
  summarise(no.beespp = n_distinct(Binomial))

#Join the two datasets together
bareground.beespp <- full_join(bee.spp, avg.bareground, by = c("Date", "Site"))

#Remove dates from bareground.bees without bare ground data
bareground.bees <- bareground.beespp %>%
  na.omit(avg.bareground)

#Fill NAs in bareground.bees with 0 (no bees were collected on these days)
bareground.beespp$no.beespp[is.na(bareground.beespp$no.beespp)] <- 0

#Create Year column
bareground.beespp$Year <- year(bareground.beespp$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#                             Years 1-2                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only 2014-2015
bareground.beespp12 <- bareground.beespp %>%
  filter(Year < 2016)

#Model for bee abundance predicted by bare ground
BGonBS12model <- glmer(no.beespp ~ avg.bareground + (1|Site) + (1|Year),
                       family = poisson,
                       data = bareground.beespp12)
summary(BGonBS12model)
AIC(BGonBS12model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBS12model)

#Change "Year" column to factor.
bareground.beespp12$Year <- as.factor(bareground.beespp12$Year)

#Find coefficients of model
coef(summary(BGonBS12model))

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBS12plot <- ggplot(bareground.beespp12,
                         aes(x = avg.bareground,
                             y = no.beespp)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonBS12model))[ , "Estimate"][1],
              slope = coef(summary(BGonBS12model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2014", "2015"),
                     values = c("#FFB90F", "#000000")) +
  scale_shape_manual(labels = c("2014", "2015"),
                     values = c(15, 16)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Bee Species Richness") +
  ggtitle("2014-2015\nInfluence of Bare Ground on \nBee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonBS12plot

#Year 3 ####
#-------------------------------------------------------------------#
#                              Year 3                               #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only 2016
bareground.beespp3 <- bareground.beespp %>%
  filter(Year == 2016)

#Model for bee abundance predicted by bare ground
BGonBS3model <- glmer(no.beespp ~ avg.bareground + (1|Site),
                      family = poisson,
                      data = bareground.beespp3)
summary(BGonBS3model)
AIC(BGonBS3model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBS12model)

#Change "Year" column to factor.
bareground.beespp3$Year <- as.factor(bareground.beespp3$Year)

#Find coefficients of model
coef(summary(BGonBS3model))

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBS3plot <- ggplot(bareground.beespp3,
                        aes(x = avg.bareground,
                            y = no.beespp)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonBS3model))[ , "Estimate"][1],
              slope = coef(summary(BGonBS3model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2016"),
                     values = c("red3")) +
  scale_shape_manual(labels = c("2016"),
                     values = c(17)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Bee Species Richness") +
  ggtitle("2016\nInfluence of Bare Ground on \nBee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonBS3plot


#Years 4-5 ####
#-------------------------------------------------------------------#
#                             Years 4-5                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only years 2017-2018
bareground.beespp45 <- bareground.beespp %>%
  filter(Year > 2016)

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonBS45model <- glmer(no.beespp ~ avg.bareground + (1|Site) + (1|Year),
                       family = poisson,
                       data = bareground.beespp45)
summary(BGonBS45model)
AIC(BGonBS45model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBS45model)

#Change "Year" column to factor.
bareground.beespp45$Year <- as.factor(bareground.beespp45$Year)

#Find coefficients of model
coef(summary(BGonBS45model))

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBS45plot <- ggplot(bareground.beespp45,
                         aes(x = avg.bareground,
                             y = no.beespp)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonBS45model))[ , "Estimate"][1],
              slope = coef(summary(BGonBS45model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2017", "2018"),
                     values = c("palegreen4", "orchid2")) +
  scale_shape_manual(labels = c("2017", "2018"),
                     values = c(18, 8)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Bee Species Richness") +
  ggtitle("2017-2018\nInfluence of Bare Ground on \nBee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonBS45plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonBSmodel <- lmer(no.beespp ~ avg.bareground + (1|Site) + (1|Year),
                    data = bareground.beespp)
summary(BGonBSmodel)
AIC(BGonBSmodel)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBSmodel)

#Change "Year" column to factor.
bareground.beespp$Year <- as.factor(bareground.beespp$Year)

#Find coefficients of model for graph
coef(summary(BGonBSmodel))

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBS12345plot <- ggplot(bareground.beespp,
                            aes(x = avg.bareground,
                                y = no.beespp)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonBSmodel))[ , "Estimate"][1],
              slope = coef(summary(BGonBSmodel))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c("#FFB90F", "#000000", "red3", "palegreen4", "orchid2")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c(15, 16, 17, 18, 8)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Bee Species Richness") +
  ggtitle("2014-2018\nInfluence of Bare Ground on \nBee Species Richness") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonBS12345plot

#Multipanel graph showing plots for 2014-2015, 2016, 2017-2018, and 2014-2018
BGonBS12plot.grid <- BGonBS12plot +
  expand_limits(y = c(0, 32)) +
  expand_limits(x = c(0, 70)) +
  geom_text(x = 60, y = 30,
            label = "y = 0.004x + 2.313",
            size = 4) +
  geom_text(x = 55, y = 29.2,
            label = "p = 0.365",
            size = 4) +
  theme(legend.position = "none")
BGonBS3plot.grid <- BGonBS3plot +
  expand_limits(y = c(0, 32)) +
  expand_limits(x = c(0, 70)) +
  labs(y = "") +
  geom_text(x = 60, y = 30,
            label = "y = -0.021x + 2.447",
            size = 4) +
  geom_text(x = 55, y = 29.2,
            label = "p = 0.061",
            size = 4) +
  theme(legend.position = "none")
BGonBS45plot.grid <- BGonBS45plot +
  expand_limits(y = c(0, 32)) +
  expand_limits(x = c(0, 70)) +
  labs(y = "") +
  geom_text(x = 60, y = 30,
            label = "y = 0.005x + 2.516",
            size = 4) +
  geom_text(x = 55, y = 29.2,
            label = "p = 0.209",
            size = 4) +
  theme(legend.position = "none")
BGonBS12345plot.grid <- BGonBS12345plot +
  expand_limits(y = c(0, 32)) +
  expand_limits(x = c(0, 70)) +
  labs(y = "") +
  geom_text(x = 60, y = 30,
            label = "y = -0.005x + 13.414",
            size = 4) +
  geom_text(x = 55, y = 29.2,
            label = "p = 0.890",
            size = 4)

grid.arrange(BGonBS12plot.grid, BGonBS3plot.grid, BGonBS45plot.grid, BGonBS12345plot.grid, ncol = 4)
