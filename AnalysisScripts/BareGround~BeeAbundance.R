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
library(ggplot2)
library(dplyr)
library(lme4)

#Read in data
Fulldata <- read.csv("Combined full data set.csv")

#Change column names so they're not so goofy.
colnames(Fulldata)[5] <- "Floral.Cover"
colnames(Fulldata)[6] <- "Blooming.Species"
colnames(Fulldata)[7] <- "Bare.Ground"

#Change "Year" column to a factor.
Fulldata$Year <- as.factor(Fulldata$Year)

#Subset large data set to include only 2014 and 2015 data.
years12 <- filter(Fulldata, Year <= 2)

#Calculate the average bare ground measured in each site during each year.
BG12 <- years12 %>%
  select(Site, Year, Bare.Ground) %>%
  group_by(Year, Site) %>%
  summarise(AverageBareGround = mean(Bare.Ground))

#Sum the number of bees collected in emergence traps at each site during each year.
ET12 <- years12 %>%
  select(Site, Year, Emergence.Traps.Abundance) %>%
  group_by(Year, Site) %>%
  summarise(ETrapAbundance = sum(Emergence.Traps.Abundance))

#Join BG12 and ET12 datasets into one
BGonBA12 <- full_join(BG12, ET12, by = c("Site", "Year"))

#Year column is brought in as an integer. Change to numeric for Amy's plot.
pch.list12 <- as.numeric(BGonBA12$Year)
pch.list12

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
BGonBA12model <- glmer(ETrapAbundance ~ AverageBareGround + Year + Site, family = poisson, data = BGonBA12)
summary(BGonBA12model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBAmodel12)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA12plot <- ggplot(BGonBA12, aes(x = AverageBareGround,
                                     y = ETrapAbundance)) +
  geom_point(aes(shape = Year,
                 color = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "Percent Bare Ground",
       y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBA12plot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                               Year 3                              #
#-------------------------------------------------------------------#
#Subset large data set to include only 2014 and 2015 data.
year3 <- filter(Fulldata, Year == 3)

#Calculate the average bare ground measured in each site during each year.
BG3 <- year3 %>%
  select(Site, Year, Bare.Ground) %>%
  group_by(Year, Site) %>%
  summarise(AverageBareGround = mean(Bare.Ground))

#Sum the number of bees collected in emergence traps at each site during each year.
ET3 <- year3 %>%
  select(Site, Year, Emergence.Traps.Abundance) %>%
  group_by(Year, Site) %>%
  summarise(ETrapAbundance = sum(Emergence.Traps.Abundance))

#Join BG12 and ET12 datasets into one
BGonBA3 <- full_join(BG3, ET3, by = c("Site", "Year"))

#Model for bee abundance predicted by bare ground
BGonBA3model <- glmer(ETrapAbundance ~ AverageBareGround + Year + Site, family = poisson, data = BGonBA3)
summary(BGonBA3model)

BGonBA12model <- glmer(ETrapAbundance ~ AverageBareGround + Year + (1|Site), family = poisson, data = BGonBA12)
summary(BGonBA12model)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA3model)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBAMMMplot <- ggplot(etMMM, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 3.2448811, slope = 0.2484154) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBAMMMplot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 3-4                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in the data
etfull <- read.csv("ETrapBees34.csv")
#Site = Site name
#Year = Year of study 
#BareGround = Average percentage of bare ground within ten 1 square meter quadrats, then averaged across each sample period
#Ind = Total number of bees collected from emergence traps for that year

#Year column in "etfull" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(etfull$Year)
pch.listfull

#Year column in "etfull" dataframe is brought in as an integer. Change to factor for Morgan's plot.
etfull$Year <- as.factor(etfull$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(etfull$BareGround, etfull$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.list), col = 'black')
model=lm(et$Ind~et$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(et$Ind~0+et$BareGround)
summary(model2)
abline(model2, lty="dotted")

plot(etfull$BareGround, etfull$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.list2), col = 'black')
model=lm(etfull$Ind~etfull$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(etfull$Ind~0+etfull$BareGround)
summary(model2)
abline(model2, lty="dotted")

#Model for bee abundance predicted by bare ground
BGonBAfull <- lm(Ind ~ BareGround, data = etfull)
summary(BGonBAfull)

#Find intercept and slope to plot best fit line on graph
coef(BGonBAfull)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBAfullplot <- ggplot(etfull, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 2.18171594, slope = 0.06844818) +
  scale_color_hue(labels = c("2016", "2017")) +
  scale_shape_manual(labels = c("2016", "2017"), values = c(16, 17)) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BGonBAfullplot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 1-4                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in the data
et1234 <- read.csv("ETrapBees1234.csv")
#Site = Site name
#Year = Year of study 
#BareGround = Average percentage of bare ground within ten 1 square meter quadrats, then averaged across each sample period
#Ind = Total number of bees collected from emergence traps for that year

#Year column in "et1234" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listfull <- as.numeric(et1234$Year)
pch.listfull

#Year column in "et1234" dataframe is brought in as an integer. Change to factor for Morgan's plot.
et1234$Year <- as.factor(et1234$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(et1234$BareGround, et1234$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.list), col = 'black')
model=lm(et$Ind~et$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(et$Ind~0+et$BareGround)
summary(model2)
abline(model2, lty="dotted")

plot(et1234$BareGround, et1234$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.list2), col = 'black')
model=lm(et1234$Ind~et1234$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(et1234$Ind~0+et1234$BareGround)
summary(model2)
abline(model2, lty="dotted")

#Model for bee abundance predicted by bare ground
BGonBA1234 <- lm(Ind ~ BareGround, data = et1234)
summary(BGonBA1234)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA1234)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBA1234plot <- ggplot(et1234, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 0.4831679, slope = 0.2596825) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"), values = c("darkorchid1", "darkgreen", "black", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"), values = c(15, 16, 17, 18)) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title.align = 0.5)
BGonBA1234plot
