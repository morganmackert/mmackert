#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#-------------------------------------------------------------------#

#Research Question: How does the amount of bare ground present within the strips influence bee abundance?

#Objectives:
#Create model(s) to explore relationship between bare ground abundance and bee abundance
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
  select(Date, Site, Quadrat, BareGround) %>%
  group_by(Date, Site, Quadrat) %>%
  summarise(total.bareground = BareGround[1])

#Calculate average bare ground cover for each site and date
avg.bareground <- bareground %>%
  group_by(Date, Site) %>%
  summarise(avg.bareground = mean(total.bareground), 
            number.quadrats = length(total.bareground))
#Two entries have only 9 quadrats included in calculation due to absences in the original 2014 data set

#Calculate number of bees collected via all traps
bees <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(!is.na(Date)) %>%
  count(Binomial) %>%
  group_by(Site, Date) %>%
  summarise(number.bees = sum(n))

#Calculate number of bees collected only in emergence traps
etrapbees <- Bees %>%
  group_by(Site, Date) %>%
  filter(Family != "Wasp") %>%
  filter(!is.na(Date)) %>%
  filter(Trap == "Emergence") %>%
  count(Binomial) %>%
  group_by(Site, Date) %>%
  summarise(number.etrapbees = sum(n))

#Remove dates in bareground.etrapbees without emergence trap bees
bareground.etrapbees <- bareground.etrapbees %>%
  na.omit(number.etrapbees)

#Join the two datasets together
bareground.bees <- full_join(bees, avg.bareground, by = c("Date", "Site"))
bareground.etrapbees <- full_join(etrapbees, avg.bareground, by = c("Date", "Site"))

#Fill NAs in bareground.bees with 0 (no bees were collected on these days)
bareground.bees$number.bees[is.na(bareground.bees$number.bees)] <- 0

#Create Year column in bareground.bees
bareground.bees$Year <- year(bareground.bees$Date)
bareground.etrapbees$Year <- year(bareground.etrapbees$Date)

#Years 1-2 ####
#-------------------------------------------------------------------#
#                           Years 1-2                               #
#-------------------------------------------------------------------#
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

#Years 1-3 ####
#-------------------------------------------------------------------#
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

#Years 3-4 ####
#-------------------------------------------------------------------#
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

#Year 4 ####
#-------------------------------------------------------------------#
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

#Years 1-4 ####
#-------------------------------------------------------------------#
#          Percent Bare Ground ~ Emergence Trap Bee Abundance       #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBA1234model <- lmer(ETrapAbundance ~ AverageBareGround + (1|Year) * (1|Site),
                          data = BGonBA1234)
summary(BGonETBA1234model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonETBA1234null <- lmer(ETrapAbundance ~ (1|Year) + (1|Site),
                         data = BGonBA1234)
summary(BGonETBA1234null)

#Likelihood ratio test between the full and null models
anova(BGonETBA1234null, BGonETBA1234model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonETBA1234model)

#Find intercept and slope to plot best fit line on graph
coef(BGonETBA1234model)

#Change "Year" column to factor.
bareground.bees$Year <- as.factor(bareground.bees$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonETBA1234plot <- ggplot(BGonBA1234, aes(x = AverageBareGround,
                                           y = ETrapAbundance)) +
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
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance in Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonETBA1234plot

#-------------------------------------------------------------------#
#              Percent Bare Ground ~ Total Bee Abundance            #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Subset bareground.bees to include only 2014-2017
bareground.bees1234 <- filter(bareground.bees, Year < "2018")

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonBA1234model <- lmer(number.bees ~ avg.bareground + (1|Year) * (1|Site),
                        data = bareground.bees1234)
summary(BGonBA1234model)

#Model for bee abundance predicted by bare ground without Year and Site.
BGonBA1234null <- lmer(number.bees ~ (1|Year) + (1|Site),
                        data = bareground.bees1234)
summary(BGonBA1234null)

#Likelihood ratio test between the full and null models
anova(BGonBA1234null, BGonBA1234model)

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBA1234model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA1234model)

#Change "Year" column to factor.
bareground.bees1234$Year <- as.factor(bareground.bees1234$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA1234plot <- ggplot(bareground.bees1234, aes(x = avg.bareground,
                                                  y = number.bees)) +
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
  labs(x = "Bare Ground (%)",
       y = "Total Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA1234plot

#Years 1-5 ####
#-------------------------------------------------------------------#
#              Percent Bare Ground ~ Total Bee Abundance            #
#                             Years 1-5                             #
#-------------------------------------------------------------------#

#Model for bee abundance predicted by bare ground including Year, Site, and their interaction as fixed effects.
BGonBA12345model <- lmer(number.bees ~ avg.bareground + (1|Year) * (1|Site),
                         data = bareground.bees)
summary(BGonBA12345model)
#AIC = 2384.456; p-value = 0.095947

BGonBA12345model2 <- lmer(number.bees ~ avg.bareground + Date + (1|Site) + (1|Year),
                          data = bareground.bees)
summary(BGonBA12345model2)
#AIC = 2390.418; p-value = 0.0977

BGonBA12345model3 <- lmer(number.bees ~ avg.bareground + (1|Date) + (1|Site) + (1|Year),
                          data = bareground.bees)
summary(BGonBA12345model3)
#AIC = 2383.567; p-value = 0.091470
#Model 3 has lowest AIC value! Use this one.

#Check residuals
qqnorm(resid(BGonBA12345model3))
qqline(resid(BGonBA12345model3))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BGonBA12345model3)
#R2m = 0.01291065; R2c = 0.4989396

#Change "Year" column to factor.
bareground.bees$Year <- as.factor(bareground.bees$Year)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA12345plot <- ggplot(bareground.bees,
                          aes(x = avg.bareground,
                              y = number.bees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F", "cornflowerblue")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c(15, 1, 17, 18, 25)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA12345plot

#-------------------------------------------------------------------#
#          Percent Bare Ground ~ Emergence Trap Bee Abundance       #
#                             Years 1-5                             #
#-------------------------------------------------------------------#

#Model for bee abundance predicted by bare ground including Year and Site as fixed effects.
BGonETBA12345model <- lmer(number.etrapbees ~ avg.bareground + (1|Year) + (1|Site),
                          data = bareground.etrapbees)
summary(BGonETBA12345model)
#AIC = 199.9698; p-value = 0.31005
##This model has lowest AIC value! Use this one.

BGonETBA12345model2 <- lmer(number.etrapbees ~ avg.bareground + Date + (1|Year) + (1|Site),
                           data = bareground.etrapbees)
summary(BGonETBA12345model2)
#AIC = 210.9973; p-value = 0.155

BGonETBA12345model3 <- lmer(number.etrapbees ~ avg.bareground + (1|Date) + (1|Year) + (1|Site),
                           data = bareground.etrapbees)
summary(BGonETBA12345model3)
#AIC = 199.0056; p-value = 0.9570

BGonETBA12345model4 <- lmer(number.etrapbees ~ avg.bareground + (1|Year) * (1|Site),
                           data = bareground.etrapbees)
summary(BGonETBA12345model4)
#AIC = 199.9698; p-value = 0.31005

#Use MuMIn to get R-squared value of best model
r.squaredGLMM(BGonETBA12345model)
#R2m = 0.03221127; R2c = 0.03221127

#Change "Year" column to factor.
bareground.etrapbees$Year <- as.factor(bareground.etrapbees$Year)

#Plot bare ground availability versus number of bees collected in emergence traps
BGonETBA12345plot <- ggplot(bareground.etrapbees,
                            aes(x = avg.bareground,
                                y = number.etrapbees)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F", "cornflowerblue")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     values = c(15, 1, 17, 18, 25)) +
  theme_bw() +
  labs(x = "Bare Ground (%)",
       y = "Emergence Trap Bee Abundance") +
  ggtitle("Influence of Bare Ground on \nBee Abundance in Emergence Traps") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonETBA12345plot

#Data dictionary ####
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

#Old code ####
Fulldata <- read.csv("Combined full data set.csv")

#Change column names so they're not so goofy
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"

Fulldata$Date <- mdy(Fulldata$Date)
Fulldata$Year <- year(Fulldata$Date)

#Calculate average bare ground and number of bees collected via emergence traps at each site during each sampling event.
BGonBA <- Fulldata %>%
  group_by(Date, Site, Year, Sampling.Period) %>%
  summarise(AverageBareGround = mean(Bare.Ground),
            ETrapAbundance = sum(Emergence.Traps.Abundance),
            TotalAbundance = sum(Total.Abundance))

#Condense data by combining all sampling dates for each site
BGonBAcondensed <- Fulldata %>%
  group_by(Site, Year) %>%
  summarise(AverageBareGround = mean(Bare.Ground),
            ETrapAbundance = sum(Emergence.Traps.Abundance))