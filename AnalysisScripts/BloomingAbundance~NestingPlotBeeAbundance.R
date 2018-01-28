#-------------------------------------------------------------------#
#   Blooming Forb and Weed Abundance ~ Nesting Plot Bee Abundance   #
#                              Year 4                               #
#-------------------------------------------------------------------#

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

#Change column names so they're not so goofy
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

#Subset Fulldata to include only 2017
year4 <- Fulldata %>%
  filter(Year == 2017)

#Convert "Year" to a  factor
year4$Year <- as.factor(year4$Year)

#Model for bee species richness predicted by number of blooming species
BAonNPBA4model <- lmer(Nesting.Plot.Abundance ~ Floral.Cover + (1|Site) + (1|Sampling.Period),
                       data = year4)
summary(BAonNPBA4model)

#Null model not including number of blooming species
BAonNPBA4null <- lmer(Nesting.Plot.Abundance ~ (1|Site) + (1|Sampling.Period),
                      data = year4)
summary(BAonNPBA4null)

#Likelihood ratio test between the full and null models
anova(BAonNPBA4null, BAonNPBA4model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BAonNPBA4model, residuals(BAonNPBA4model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BAonNPBA4model)

#Plot of the number of blooming forb/weed species vs. nesting plot bee species richness
BAonNPBA4plot <- ggplot(year4, 
                        aes(x = Floral.Cover,
                            y = Nesting.Plot.Abundance)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Average Blooming Forb/Weed Coverage (%)",
       y = "Number of Nesting Plot Bee Species") +
  ggtitle("Influence of Blooming Plant Abundance \non Nesting Plot Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align  = 0.5)
BAonNPBA4plot