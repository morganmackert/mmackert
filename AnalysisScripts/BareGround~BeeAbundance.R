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
setwd("~/ISU/Project/mmackert/Data")

#Read in data
etAM <- read.csv("ETrap_Bees.csv")
#Site = Site name
#Year = Year of study 
#BareGround = Average percentage of bare ground within ten 1 square meter quadrats, then averaged across each sample period
#Ind = Total number of bees collected from emergence traps for that year

#Year column in "etAM" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listAM <- as.numeric(etAM$Year)
pch.listAM

#Year column in "etAM" dataframe is brought in as an integer. Change to factor for Morgan's plot.
etAM$Year <- as.factor(etAM$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(etAM$BareGround, etAM$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.listAM), col = 'black')
model=lm(etAM$Ind~etAM$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(etAM$Ind~0+etAM$BareGround)
summary(model2)
abline(model2, lty="dotted")

#Model for bee abundance predicted by bare ground
BGonBAAM <- lm(Ind ~ BareGround, data = etAM)
summary(BGonBAAM)

#Find intercept and slope to plot best fit line on graph
coef(BGonBAAM)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBAAMplot <- ggplot(etAM, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = -0.7025809, slope = 0.3164081) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBAAMplot

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                               Year 3                              #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in the data
etMMM <- read.csv("ETrapBeesMMM.csv")
#Site = Site name
#Year = Year of study 
#BareGround = Average percentage of bare ground within ten 1 square meter quadrats, then averaged across each sample period
#Ind = Total number of bees collected from emergence traps for that year

#Year column in "etMMM" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.listMMM <- as.numeric(etMMM$Year)
pch.listMMM

#Year column in "etMMM" dataframe is brought in as an integer. Change to factor for Morgan's plot.
etMMM$Year <- as.factor(etMMM$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(etMMM$BareGround, etMMM$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.listMMM), col = 'black')
model=lm(etMMM$Ind~etMMM$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(etMMM$Ind~0+etMMM$BareGround)
summary(model2)
abline(model2, lty="dotted")

#Model for bee abundance predicted by bare ground
BGonBAMMM <- lm(Ind ~ BareGround, data = etMMM)
summary(BGonBAMMM)

#Find intercept and slope to plot best fit line on graph
coef(BGonBAMMM)

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
