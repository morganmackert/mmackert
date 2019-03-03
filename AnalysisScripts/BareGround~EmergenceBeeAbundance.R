#-------------------------------------------------------------------#
#        Percent Bare Ground ~ Emergence Trap Bee Abundance         #
#-------------------------------------------------------------------#

#Research Question: How does the amount of bare ground present within the strips influence emergence trap bee abundance?

#Objectives:
#Create model(s) to explore relationship between bare ground abundance and emergence trap bee abundance
#Use created model(s) to visualize the relationship graphically

#Start ####
#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(ggplot2)
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

#Calculate number of bees collected via emergence traps
et.bees <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Wasp") %>%
  filter(Trap == "Emergence") %>%
  count(Binomial) %>%
  group_by(Site, Date) %>%
  summarise(number.bees = sum(n))

#Join the two datasets together
bareground.etbees <- left_join(et.bees, avg.bareground, by = c("Date", "Site"))

#Create Year column in bareground.bees
bareground.etbees$Year <- year(bareground.etbees$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#                             Years 1-2                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only 2014-2015
bareground.etbees12 <- bareground.etbees %>%
  filter(Year < 2016)

#Model for bee abundance predicted by bare ground
BGonETBA12model <- glm(number.bees ~ avg.bareground,
                       family = poisson,
                       data = bareground.etbees12)
summary(BGonETBA12model)
AIC(BGonETBA12model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA12model)

#Change "Year" column to factor.
bareground.etbees12$Year <- as.factor(bareground.etbees12$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA12plot <- ggplot(bareground.etbees12,
                         aes(x = avg.bareground,
                             y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonETBA12model))[ , "Estimate"][1],
              slope = coef(summary(BGonETBA12model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2014", "2015"),
                     values = c("#FFB90F", "#000000")) +
  scale_shape_manual(labels = c("2014", "2015"),
                     values = c(15, 16)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance \nin Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonETBA12plot

#Year 3 ####
#-------------------------------------------------------------------#
#                              Year 3                               #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only 2016
bareground.etbees3 <- bareground.etbees %>%
  filter(Year == 2016)

#Model for bee abundance predicted by bare ground
BGonETBA3model <- glm(number.bees ~ avg.bareground,
                      family = poisson,
                      data = bareground.etbees3)
summary(BGonETBA3model)
AIC(BGonETBA3model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA12model)

#Change "Year" column to factor.
bareground.etbees3$Year <- as.factor(bareground.etbees3$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA3plot <- ggplot(bareground.etbees3,
                         aes(x = avg.bareground,
                             y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonETBA3model))[ , "Estimate"][1],
              slope = coef(summary(BGonETBA3model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2016"),
                     values = c("red3")) +
  scale_shape_manual(labels = c("2016"),
                     values = c(17)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance \nin Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonETBA3plot

#Years 1-4 ####
#-------------------------------------------------------------------#
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only years 2014-2017
bareground.etbees1234 <- filter(bareground.etbees, Year < "2018")

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBA1234model <- glm(number.bees ~ avg.bareground + (1|Year) + (1|Site),
                           family = poisson,
                           data = bareground.etbees1234)
summary(BGonETBA1234model)
AIC(BGonETBA1234model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonETBA1234null <- lmer(number.bees ~ (1|Year) + (1|Site),
                         data = bareground.etbees1234)
summary(BGonETBA1234null)

#Likelihood ratio test between the full and null models
anova(BGonETBA1234null, BGonETBA1234model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA1234model)

#Find intercept and slope to plot best fit line on graph
coef(BGonETBA1234model)

#Change "Year" column to factor.
bareground.etbees1234$Year <- as.factor(bareground.etbees1234$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA1234plot <- ggplot(bareground.etbees1234,
                           aes(x = avg.bareground,
                               y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonETBA1234model))[ , "Estimate"][1],
              slope = coef(summary(BGonETBA1234model))[ , "Estimate"][2]) +
  geom_text(x = 6.2, y = 13,
            label = "y = 0.071x + 2.344",
            size = 4) +
  geom_text(x = 3, y = 12.5,
            label = "p = 0.501",
            size = 4) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c("#FFB90F", "#000000", "red3", "palegreen4")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance \nin Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonETBA1234plot

#Years 4-5 ####
#-------------------------------------------------------------------#
#                             Years 4-5                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only years 2017-2018
bareground.etbees45 <- bareground.etbees %>%
  filter(Year > 2016)

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBA45model <- glm(number.bees ~ avg.bareground,
                       family = poisson,
                       data = bareground.etbees45)
summary(BGonETBA45model)
AIC(BGonETBA45model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA45model)

#Change "Year" column to factor.
bareground.etbees45$Year <- as.factor(bareground.etbees45$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA45plot <- ggplot(bareground.etbees45,
                           aes(x = avg.bareground,
                               y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonETBA45model))[ , "Estimate"][1],
              slope = coef(summary(BGonETBA45model))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2017", "2018"),
                     values = c("palegreen4", "orchid2")) +
  scale_shape_manual(labels = c("2017", "2018"),
                     values = c(18, 8)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance \nin Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonETBA45plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBAmodel <- glm(number.bees ~ avg.bareground,
                       data = bareground.etbees)
summary(BGonETBAmodel)
AIC(BGonETBAmodel)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBAmodel)

#Change "Year" column to factor.
bareground.etbees$Year <- as.factor(bareground.etbees$Year)

#Find coefficients of model for graph
coefficients(BGonETBAmodel)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA12345plot <- ggplot(bareground.etbees,
                           aes(x = avg.bareground,
                               y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_abline(intercept = coef(summary(BGonETBAmodel))[ , "Estimate"][1],
              slope = coef(summary(BGonETBAmodel))[ , "Estimate"][2]) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c("#FFB90F", "#000000", "red3", "palegreen4", "orchid2")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c(15, 16, 17, 18, 8)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance \nin Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5) 
BGonETBA12345plot

#Multipanel graph showing plots for 2014-2015, 2016, 2017-2018, and 2014-2018
BGonETBA12plot.grid <- BGonETBA12plot +
  expand_limits(y = c(0, 20)) +
  expand_limits(x = c(0, 40)) +
  geom_text(x = 6.2, y = 20,
            label = "y = 0.023x + 0.960",
            size = 4) +
  geom_text(x = 3, y = 19.5,
            label = "p = 0.065",
            size = 4) +
  theme(legend.position = "none") +
  ggtitle("2014-2015\nInfluence of Bare Ground on\nEmergence Trap Bee Abundance")
BGonETBA3plot.grid <- BGonETBA3plot +
  expand_limits(y = c(0, 20)) +
  expand_limits(x = c(0, 40)) +
  labs(y = "") +
  geom_text(x = 6.2, y = 20,
            label = "y = -0.113x + 1.421",
            size = 4) +
  geom_text(x = 3, y = 19.5,
            label = "p = 0.079",
            size = 4) +
  theme(legend.position = "none") +
  ggtitle("2016\nInfluence of Bare Ground on\nEmergence Trap Bee Abundance")
BGonETBA45plot.grid <- BGonETBA45plot +
  expand_limits(y = c(0, 20)) +
  expand_limits(x = c(0, 40)) +
  labs(y = "") +
  geom_text(x = 6.2, y = 20,
            label = "y = 1.090x + -0.464",
            size = 4) +
  geom_text(x = 3, y = 19.5,
            label = "p < 0.001",
            size = 4) +
  theme(legend.position = "none") +
  ggtitle("2017-2018\nInfluence of Bare Ground on\nEmergence Trap Bee Abundance")
BGonETBA12345plot.grid <- BGonETBA12345plot +
  expand_limits(y = c(0, 20)) +
  expand_limits(x = c(0, 40)) +
  labs(y = "") +
  geom_text(x = 6.2, y = 20,
            label = "y = 0.070x + 2.800",
            size = 4) +
  geom_text(x = 3, y = 19.5,
            label = "p = 0.357",
            size = 4) +
  ggtitle("2014-2018\nInfluence of Bare Ground on\nEmergence Trap Bee Abundance")

grid.arrange(BGonETBA12plot.grid, BGonETBA3plot.grid, BGonETBA45plot.grid, BGonETBA12345plot.grid, ncol = 4)

#MRPP ####
#-------------------------------------------------------------------#
#           MRPP:  Emergence Trap Bee Community by Year             #
#-------------------------------------------------------------------#
#Compare bee communities between 2014, 2015-2016, and 2017-2018 to address any differences between different timing of emergence trap deployment
etbees <- Bees %>%
  group_by(Site, Date, Year) %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Wasp") %>%
  filter(Trap == "Emergence") %>%
  count(Binomial)

#Redo Year naming conventions to include different emergence trap deployment times
etbees$Year <- as.character(etbees$Year)
etbees$Year[etbees$Year == "2015"] <- "201516"
etbees$Year[etbees$Year == "2016"] <- "201516"
etbees$Year[etbees$Year == "2017"] <- "201718"
etbees$Year[etbees$Year == "2018"] <- "201718"

#Reformat from long to wide
etbees.wide <- spread(etbees, Binomial, n)

#Fill NAs with 0
etbees.wide[is.na(etbees.wide)] <- 0

#Move Year column to another data frame
etbees.wideyear <- etbees.wide["Year"]

#Remove extra columns
etbees.wide <- etbees.wide[!names(etbees.wide) %in% c("Site", "Date", "Year")]

#Convert to data.frame
etbees.wide <- as.data.frame(etbees.wide)

#MRPP
etbees.mrpp <- mrpp(etbees.wide, etbees.wideyear$Year, distance = "bray")
etbees.mrpp