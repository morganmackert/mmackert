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

#Years 1-4 ####
#-------------------------------------------------------------------#
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Subset bareground.etbees to include only years 2014-2017
bareground.etbees1234 <- filter(bareground.etbees, Year < "2018")

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBA1234model <- lmer(number.bees ~ avg.bareground + (1|Year) * (1|Site),
                          data = bareground.etbees1234)
summary(BGonETBA1234model)

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
BGonETBA1234plot <- ggplot(bareground.etbees1234, aes(x = avg.bareground,
                                                      y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
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

#Years 1-5 ####
#-------------------------------------------------------------------#
#                             Years 1-5                             #
#-------------------------------------------------------------------#
#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBAmodel <- lmer(number.bees ~ avg.bareground + (1|Year) * (1|Site),
                      data = bareground.etbees)
summary(BGonETBAmodel)
AIC(BGonETBAmodel)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA1234model)

#Change "Year" column to factor.
bareground.etbees1234$Year <- as.factor(bareground.etbees1234$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA1234plot <- ggplot(bareground.etbees1234, aes(x = avg.bareground,
                                                      y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
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