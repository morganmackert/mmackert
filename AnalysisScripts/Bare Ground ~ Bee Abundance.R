#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                               Year 3                              #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Set working directory
setwd("~/ISU/Project/mmackert/DataWrangling")

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
ggplot(etMMM, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 3.2448811, slope = 0.2484154) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#                             Years 1-3                             #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in the data
etfull <- read.csv("ETrapBeesCombined.csv")
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
ggplot(etfull, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 0.6387116, slope = 0.2785092) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
