#################################################
################# BEES VS PLANTS ################
#################################################

#Research question: How does blooming floral abundance (average coverage) influence bee abundance?

#Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(lme4)

# Load data
bees <- read.csv("https://raw.githubusercontent.com/goodekat/mmackert/master/Data/bees/raw/2016Bees.csv")
plants <- read.csv("https://raw.githubusercontent.com/goodekat/mmackert/master/Data/plants/raw/2016Total.csv")

#------------------------------------------------
#               Data Manipulation
#------------------------------------------------
#Bees: merge traps into one total number for each site/date
merged_bees <- bees %>% 
  filter(Trap != "Total") %>%
  group_by(Date, Site) %>%
  summarise(Total_Bees = sum(Bees),
            Number_Traps = length(Bees))

#Plants: fill NA values with zero values.
##### KATHERINE: Is this okay to do?
plants[is.na(plants)] <- 0

#Plants: determine average coverage of each quadrat, then average of the strip.
##### KATHERINE: Is this an appropriate way to test this???
plantcoverage_averaged <- plants %>%
  select(Date, Site, Quadrat, X..Cover) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(Percent_Coverage = X..Cover[1]) %>%
  group_by(Date, Site) %>%
  summarise(Average_Coverage = mean(Percent_Coverage), 
            Number_Quadrats = length(Percent_Coverage))

#Join merged bees and averaged plants together
##### KATHERINE: Same full join error as before.
plantcoverage_data <- full_join(merged_bees, plantcoverage_averaged, by = c("Date", "Site"))

#Fix dates with lubridate
mdy(plantcoverage_data$Date)

#Drop 8/5/2016 observation from Cretsinger 
##### Change this to 7/27/2016
plantcoverage_data <- plantcoverage_data %>%
  mutate(Site = as.factor(Site)) %>%
  filter(Date != "8/5/2016") %>%
  arrange(Site, Date)

#Create new variable to denote sampling period (1, 2, 3, 4, 5)
plantcoverage_data <- plantcoverage_data %>% 
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Create new variable to denote sampling period (Early May, Late May, ...)
##### KATHERINE: Is there a better way to do this?
plantcoverage_data <- plantcoverage_data %>%
  group_by(Site) %>%
  mutate(Sampling_Period = as.factor(dense_rank(Date)))
plantcoverage_data$Sampling_Period = as.character(plantcoverage_data$Sampling_Day)
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 1] <- "Early May"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 2] <- "Late May"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 3] <- "June"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 4] <- "July"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 5] <- "August"

##### KATHERINE: Looks okay?

#------------------------------------------------
#               Figure Scripts
#------------------------------------------------
#Plot of total bees versus average plant coverage
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point()

#Plot of total bees versus average plant coverage faceted by site
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point() + 
  facet_wrap( ~ Site)

# Plot of total bees versus sampling day faceted by site
##### KATHERINE: How to make the sampling period appear in order instead of alphabetically? Google is failing me.
ggplot(plantcoverage_data, aes(x = Sampling_Period, y = Total_Bees, color = Site)) + 
  geom_line() +
  geom_point() +
  facet_wrap( ~ Site)

# Plot of total bees versus average plant coverage faceted by sampling day and colored by site
##### KATHERINE: Same issue, how to reorder?
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point( ) +
  facet_wrap( ~ Sampling_Period)

#------------------------------------------------
#                    Models
#------------------------------------------------
# Poisson mixed model with sampling day and average plant coverage as predictors
fit <- glmer(Total_Bees ~ Average_Coverage + Sampling_Day + Average_Coverage * Sampling_Day + 
               (1|Site),
             data = plantcoverage_data, family = poisson(link = "log"))






