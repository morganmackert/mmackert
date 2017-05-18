#################################################
################ BEES VS. PLANTS ################
#################################################

#Research question: How does blooming floral abundance (average coverage) within contour buffer and filter strips of various vegetation mixes influence bee abundance?

#Clear environment
rm(list=ls())

#Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(lme4)
library(phia)

#Load data
bees <- read.csv("https://raw.githubusercontent.com/morganmackert/mmackert/master/Data/bees/working/2016Bees.csv")
plants <- read.csv("https://raw.githubusercontent.com/morganmackert/mmackert/master/Data/plants/raw/2016Total.csv")

#------------------------------------------------#
#               Data Manipulation                #
#------------------------------------------------#
# Create new datasets that can be joined to have a dataset with variables on number of bees and number of blooming forb and weed species

#Bees: merge traps into one total number for each site/date
merged_bees <- bees %>% 
  filter(Trap != "Total") %>%
  group_by(Date, Site) %>%
  summarise(Total_Bees = sum(Bees),
            Number_Traps = length(Bees))

#Plants: rename goofy column headings
plants <- setNames(plants, c("Date","Site","Quadrat", "Species", "Percent_Cover", "Percent_Bare_Ground", "Species_in_Strip", "Outside_Species"))

#Plants: fill NA values with zero values.
plants[is.na(plants)] <- 0

#Plants: determine average coverage of each quadrat, then average of the strip.
plantcoverage_averaged <- plants %>%
  select(Date, Site, Quadrat, Percent_Cover) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(Percent_Coverage = sum(Percent_Cover)) %>%
  group_by(Date, Site) %>%
  summarise(Average_Coverage = mean(Percent_Coverage), 
            Number_Quadrats = length(Percent_Coverage))

#Join merged bees and averaged plants together
##### JOHN: Help with full join error please!
plantcoverage_data <- full_join(merged_bees, plantcoverage_averaged, by = c("Date", "Site"))

#Fix dates with lubridate
##### JOHN: Julian date rather than typical date format?
mdy(plantcoverage_data$Date)

#Check to see how many dates per site; should be five each
plantcoverage_data %>%
  group_by(Site) %>%
  summarise(count = n())

#Cretsinger has six!
#Drop 8/5/2016 observation from Cretsinger 
##### JOHN: Change this to 7/27/2016? How do I choose which date to drop? Delete from original? Filter with multiple options?
plantcoverage_data <- plantcoverage_data %>%
  mutate(Site = as.factor(Site)) %>%
  filter(Date != "8/5/2016") %>%
  arrange(Site, Date)

#Create new variable to denote sampling period (1, 2, 3, 4, 5)
plantcoverage_data <- plantcoverage_data %>% 
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Create new variable to denote sampling period (Early May, Late May, ...)
plantcoverage_data <- plantcoverage_data %>%
  group_by(Site) %>%
  mutate(Sampling_Period = as.factor(dense_rank(Date)))
plantcoverage_data$Sampling_Period = as.character(plantcoverage_data$Sampling_Day)
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 1] <- "Early May"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 2] <- "Late May"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 3] <- "June"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 4] <- "July"
plantcoverage_data$Sampling_Period[plantcoverage_data$Sampling_Period == 5] <- "August"

plantcoverage_data$Sampling_Period <- factor(plantcoverage_data$Sampling_Period,
                                             levels = c("Early May", "Late May", "June", "July", "August"))

#------------------------------------------------#
#               Figure Scripts
#------------------------------------------------#
#Plot of total bees versus average plant coverage by site
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point(shape = 19, size = 3) +
  ggtitle("Bee Abundance vs. Flowering Plant Average Coverage by Site") +
  labs(x = "Flowering Plant Average Coverage", y = "Bee Abundance") +
  theme(legend.title = element_text(color = "black", size = 12, face = NULL)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  

#Plot of total bees versus average plant coverage by sampling period
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = (Sampling_Period))) +
  geom_point(shape = 19, size = 3) +
  ggtitle("Bee Abundance vs. Flowering Plant Average Coverage by Sampling Period") +
  labs(x = "Flowering Plant Average Coverage", y = "Bee Abundance") +
  theme(legend.title = element_text(color = "black", size = 12, face = NULL)) + 
  scale_color_discrete(name = "Sample Period", breaks = c("Early May", "Late May", "June", "July", "August")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#Plot of total bees versus average plant coverage faceted by site
##### Need legend here? Seems redundant.
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point() + 
  facet_wrap( ~ Site) +
  ggtitle("Bee Abundance vs. Flowering Plant Average Coverage by Site") +
  labs(x = "Flowering Plant Average Coverage", y = "Bee Abundance") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
  
#Plot of total bees versus sampling day faceted by site
##### Rotate x-axis labels
ggplot(plantcoverage_data, aes(x = Sampling_Period, y = Total_Bees, color = Site)) +
  geom_point() +
  facet_wrap( ~ Site) +
  geom_point(shape = 19, size = 3) +
  ggtitle("Bee Abundance vs. Sampling Period by Site") +
  labs(x = "Sampling Period", y = "Bee Abundance") +
  theme(legend.title = element_text(color = "black", size = 12, face = NULL)) + 
  scale_color_discrete(name = "Sample Period", breaks = c("Early May", "Late May", "June", "July", "August")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot of total bees versus average plant coverage faceted by sampling day and colored by site
ggplot(plantcoverage_data, aes(x = Average_Coverage, y = Total_Bees, color = Site)) +
  geom_point(shape = 19, size  = 3 ) +
  ggtitle("Bee Abundance vs. Flowering Plant Average Coverage \n by Sampling Period and Site") +
  labs(x = "Flowering Plant Average Coverage", y = "Bee Abundance") +
  facet_wrap( ~ Sampling_Period) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------#
#                    Models
#------------------------------------------------#
#Poisson mixed model with sampling day and average plant coverage as predictors
model <- glmer(Total_Bees ~ Average_Coverage + Sampling_Day + Average_Coverage * Sampling_Day + (1|Site),
             data = plantcoverage_data, family = poisson(link = "log"))

plantcoverage_data$Average_Coverage_Scaled <- (plantcoverage_data$Average_Coverage - mean(plantcoverage_data$Average_Coverage)) / sd(plantcoverage_data$Average_Coverage)

model <- glmer(Total_Bees ~ Average_Coverage_Scaled + Sampling_Day + Average_Coverage_Scaled * Sampling_Day + (1|Site),
               data = plantcoverage_data, family = poisson(link = "log"))

fit <- glm(Total_Bees ~ Average_Coverage + Sampling_Day + Average_Coverage * Sampling_Day + Site,
    data = plantcoverage_data, family = poisson(link = "log"))

fit2 <- glm(Total_Bees ~ Sampling_Day + Site+ Site* Sampling_Day,
           data = plantcoverage_data, family = poisson(link = "log"))

summary(fit)
summary(fit2)
summary(model)
plot(interactionMeans(fit))
plot(interactionMeans(fit2))
anova(model)


#------------------------------------------------#
#                   Extras
#------------------------------------------------#
plantcoverage_data$Sampling_Period <- as.factor(plantcoverage_data$Sampling_Period)

plantcoverage_data$Sampling_Period <- factor(plantcoverage_data$Sampling_Period, levels(plantcoverage_data$Sampling_Period)[c(2,5,4,3,1)])

levels(plantcoverage_data$Sampling_Period)



