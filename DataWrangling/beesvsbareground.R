#################################################
############# BEES VS. BARE GROUND ##############
#################################################

#Research question: How does bare ground presence within contour buffer and filter strips of various vegetation mixes influence bee abundance?

# Load libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(lubridate)
library(lsmeans)

# Load data
bees <- read.csv("https://raw.githubusercontent.com/morganmackert/mmackert/master/Data/bees/working/2016Bees.csv")
plants <- read.csv("https://raw.githubusercontent.com/morganmackert/mmackert/master/Data/plants/raw/2016Total.csv")


#------------------------------------------------
#               Data Manipulation
#------------------------------------------------
#Create new datasets that can be joined to have a dataset with variables on number of bees and bare ground coverage

#Bees: merge traps into one total number for each site/date
merged_bees <- bees %>% 
  filter(Trap != "Total") %>%
  group_by(Date, Site) %>%
  summarise(Total_Bees = sum(Bees),
            Number_Traps = length(Bees))

#Plants: rename goofy column headings
plants <- setNames(plants, c("Date","Site","Quadrat", "Species", "Percent_Cover", "Percent_Bare_Ground", "Species_in_Strip", "Outside_Species"))

#Plants: determine average bare ground coverage of each quadrat
bareground_averaged <- plants %>%
  select(Date, Site, Quadrat, Percent_Bare_Ground) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(Percent_Bare_Ground = Percent_Bare_Ground[1]) %>%
  group_by(Date, Site) %>%
  summarise(Average_Bare_Ground = mean(Percent_Bare_Ground), 
            Number_Quadrats = length(Percent_Bare_Ground))

#Join merged bees and averaged bare ground together
bareground_data <- full_join(merged_bees, bareground_averaged, by = c("Date", "Site")) 

#Fix dates with lubridate
mdy(bareground_data$Date)

# Check to see how many dates per site (we find the Crestsinger has 6)
bareground_data %>%
  group_by(Site) %>%
  summarise(count = n())

#Cretsinger has six!
#Drop 8/5/2016 observation from Cretsinger
bareground_data <- bareground_data %>%
  mutate(Site = as.factor(Site)) %>%
  filter(Date != "8/5/2016") %>%
  arrange(Site, Date)

#Create new variable to denote sampling period (1, 2, 3, 4, 5)
bareground_data <- bareground_data %>% 
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Create new variable to denote sampling period (Early May, Late May, ...)
bareground_data <- bareground_data %>%
  group_by(Site) %>%
  mutate(Sampling_Period = as.factor(dense_rank(Date)))
bareground_data$Sampling_Period = as.character(bareground_data$Sampling_Day)
bareground_data$Sampling_Period[bareground_data$Sampling_Period == 1] <- "Early May"
bareground_data$Sampling_Period[bareground_data$Sampling_Period == 2] <- "Late May"
bareground_data$Sampling_Period[bareground_data$Sampling_Period == 3] <- "June"
bareground_data$Sampling_Period[bareground_data$Sampling_Period == 4] <- "July"
bareground_data$Sampling_Period[bareground_data$Sampling_Period == 5] <- "August"

#Create new variable to denote sample number
bareground_data$Sample_Number <- 1:40

#------------------------------------------------#
#                 Figure Scripts
#------------------------------------------------#

#Plot of total bees versus average plant coverage by site
ggplot(bareground_data, aes(x = Average_Bare_Ground, y = Total_Bees, color = Site)) +
  geom_point() 

#Plot of total bees versus average bare ground faceted by site
ggplot(bareground_data, aes(x = Average_Bare_Ground, y = Total_Bees, color = Site)) +
  geom_point() + 
  facet_wrap( ~ Site)

#Plot of total bees versus sampling day faceted by site
ggplot(bareground_data, aes(x = as.numeric(Sampling_Day), y = Total_Bees, color = Site)) + 
  geom_line() +
  geom_point() +
  facet_wrap( ~ Site)

# Plot of total bees versus average bare ground faceted by sampling day and colored by site
ggplot(bareground_data, aes(x = Average_Bare_Ground, y = Total_Bees, color = Site)) +
  geom_point() +
  facet_wrap( ~ Sampling_Day)


#------------------------------------------------#
#                    Models
#------------------------------------------------#

#Poisson mixed model with sampling day and bare ground as predictors
model <- glmer(Total_Bees ~ Average_Bare_Ground + Sampling_Day + Average_Bare_Ground * Sampling_Day + (1|Site),
             data = bareground_data, family = poisson(link = "log"))

#Summary and ANOVA of model
summary(model)
anova(model)  

#LSmeans by sampling day (log and original scale)
lsmeans(model, "Sampling_Day")
lsmeans(model, "Sampling_Day", type = "response")

#Deviance and Pearson residuals
d <- resid(model, type = "deviance")
p <- resid(model, type = "pearson")

#Plots of residuals
plot(d ~ fitted(model))
abline(h = 0)

#Deviance
deviance(model)

#Tests for overdispersion
1-pchisq(deviance(model), df.residual(model))
1-pchisq(sum(p^2), df.residual(model))

#Estimates of overdispersion parameter
deviance(model)/df.residual(model)
sum(p^2)/df.residual(model)

#Attempt to fit a quasi-Poisson model
q.model <- glmer(Total_Bees ~ Average_Bare_Ground + Sampling_Day + Average_Bare_Ground * Sampling_Day + 
                 (1|Site),
               data = bareground_data, family = quasipoisson(link = "log"))

# Model with random effect for site and every sample
##### FAILURE TO CONVERGE
r.model <- glmer(Total_Bees ~ Average_Bare_Ground + Sampling_Day + Average_Bare_Ground * Sampling_Day + 
                 (1|Site) + (1|Sample_Number),
               data = bareground_data, family = poisson(link = "log"))

# Summary of model
summary(r.model)

# Anova table from model
anova(r.model)

# LSmeans by sampling day (log and original scale)
lsmeans(r.model, "Sampling_Day")
lsmeans(r.model, "Sampling_Day", type = "response")

# Deviance and Pearson residuals
r.d <- resid(r.model, type = "deviance")

# Plots of residuals
plot(r.d ~ fitted(model))
abline(h = 0)