#####################################################################
#                POISSON MIXED MODEL - BARE GROUND                  #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Read in data
bees <- read.csv("bees/working/2016Bees.csv")
plants <- read.csv("plants/working/2016Total.csv")

#Load libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lsmeans)

#-------------------------------------------------------------------#
#                          Data Manipulation                        #
#-------------------------------------------------------------------#

# Create new datasets that can be joined to have a dataset with variables on 
# number of bees and bare ground

# %>% pipeline operator (only in dplyr)

# Compute sum of bees in all traps at a site on a particular day
bees_summarised <- bees %>% 
  filter(Trap != "Total") %>%
  group_by(Date, Site) %>%
  summarise(Total_Bees = sum(Bees),
            Number_Traps = length(Bees))

# Compute average percent of bare gound of 10 quadrats at a site on a particular day
bareground_averaged <- plants %>%
  select(Date, Site, Quadrat, X..Bare.Ground..Bee.s.eye.view.) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(Percent_BareGround = X..Bare.Ground..Bee.s.eye.view.[1]) %>%
  group_by(Date, Site) %>%
  summarise(Average_BareGround = mean(Percent_BareGround), 
            Number_Quadrats = length(Percent_BareGround))

# Join the two datasets together
baregound_data <- full_join(bees_summarised, bareground_averaged, by = c("Date", "Site")) 

# Use lubridate to allow R to recognize the dates
mdy(baregound_data$Date)

# Check to see how many dates per site (we find the Crestsinger has 6)
baregound_data %>%
  group_by(Site) %>%
  summarise(count = n())

# Dropped 8/5/2016 observation from Cretsinger (need to change to 7/27/2016)
baregound_data <- baregound_data %>%
  mutate(Site = as.factor(Site)) %>%
  filter(Date != "8/5/2016") %>%
  arrange(Site, Date)

# Create a new variable for the sampling day
baregound_data <- baregound_data %>% 
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

# Create an ID variable for site by sampling day
baregound_data$Sample <- 1:40


## ===================================================================================
## Graphs
## ===================================================================================

# Plot of total bees versus average bare ground
ggplot(baregound_data, aes(x = Average_BareGround, y = Total_Bees, color = Site)) +
  geom_point() 

# Plot of total bees versus average bare ground faceted by site
ggplot(baregound_data, aes(x = Average_BareGround, y = Total_Bees, color = Site)) +
  geom_point() + 
  facet_wrap( ~ Site)

# Plot of total bees versus sampling day faceted by site
ggplot(baregound_data, aes(x = as.numeric(Sampling_Day), y = Total_Bees, color = Site)) + 
  geom_line() +
  geom_point() +
  facet_wrap( ~ Site)

# Plot of total bees versus average bare ground faceted by sampling day and colored by site
ggplot(baregound_data, aes(x = Average_BareGround, y = Total_Bees, color = Site)) +
  geom_point( ) +
  facet_wrap( ~ Sampling_Day)


## ===================================================================================
## Poisson Mixed Models
## ===================================================================================

# Poisson mixed model with sampling day and bare ground as predictors
fit <- glmer(Total_Bees ~ Average_BareGround + Sampling_Day + Average_BareGround * Sampling_Day + (1|Site),
             data = baregound_data, family = poisson(link = "log"))

# Summary of model
summary(fit)

# Anova table
anova(fit)  

# LSmeans by sampling day (log and original scale)
lsmeans(fit, "Sampling_Day")
lsmeans(fit, "Sampling_Day", type = "response")

# Deviance and Pearson residuals
d <- resid(fit, type = "deviance")
p <- resid(fit, type = "pearson")

# Plots of residuals
plot(d ~ fitted(fit))
abline(h = 0)

# Deviance
deviance(fit)

# Tests for overdispersion
1-pchisq(deviance(fit), df.residual(fit))
1-pchisq(sum(p^2), df.residual(fit))

# Estimates of overdispersion parameter
deviance(fit)/df.residual(fit)
sum(p^2)/df.residual(fit)

# Attempt to fit a quasi-Poisson model
q.fit <- glmer(Total_Bees ~ Average_BareGround + Sampling_Day + Average_BareGround * Sampling_Day + 
                 (1|Site),
               data = baregound_data, family = quasipoisson(link = "log"))

# Model with random effect for site and every sample
r.fit <- glmer(Total_Bees ~ Average_BareGround + Sampling_Day + Average_BareGround * Sampling_Day + 
                 (1|Site) + (1|Sample),
               data = baregound_data, family = poisson(link = "log"))

# Summary of model
summary(r.fit)

# Anova table from model
anova(r.fit)

# LSmeans by sampling day (log and original scale)
lsmeans(r.fit, "Sampling_Day")
lsmeans(r.fit, "Sampling_Day", type = "response")

# Deviance and Pearson residuals
r.d <- resid(r.fit, type = "deviance")

# Plots of residuals
plot(r.d ~ fitted(fit))
abline(h = 0)
