###############################################################
################   BEES VS. PLANTS FIGURES  ###################
###############################################################

#Research question: How do floral resources influence bee abundance?

#Load libraries
library(ggplot2)
library(lubridate)
library(lme4)
library(tidyr)

#Read in data
TotalsByPeriod <- read.csv("~/ISU/Semester 3/R/mmackert/Data/TotalsByPeriod.csv")

#Plot number of bees/plants by site
with(beesbysite, plot(Num ~ Site, ylab = "Bee Abundance"))
with(plantsbysite, plot(Num ~ Site, ylab = "Flowering Plant Richness"))

#Plot number of bees/plants by date; include best fit line
with(beesbysite, plot(Num ~ Date, ylab = "Bee Abundance"));
with(beesbysite, abline(lm(Num ~ Date)))
with(plantsbysite, plot(Num ~ Date, ylab = "Flowering Plant Richness"));
with(plantsbysite, abline(lm(Num ~ Date)))

#Plot using ggplot2
#Total bees
beesbysiteplot <- ggplot(beesbysite, aes(x = Date, y = Num)) + 
  geom_point(shape = 19, size = 2) + 
  geom_smooth(method = lm) +
  ggtitle("Bee Abundance") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Individuals")
beesbysiteplot

#Number of bees color coded by "Site"
beesbysiteplotcolorsmooth <- ggplot(beesbysite, aes(x = Date, y = Num, color = Site)) + 
  geom_point(shape = 19, size = 2) + 
  geom_smooth(method = lm) +
  ggtitle("Bee Abundance") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Individuals")
beesbysiteplotcolorsmooth

beesbysiteplotcolorline <- ggplot(beesbysite, aes(x = Date, y = Num, color = Site)) + 
  geom_point(shape = 19, size = 2) +
  geom_line() +
  ggtitle("Bee Abundance") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Individuals")
beesbysiteplotcolorline

#Total plants
plantsbysiteplot <- ggplot(plantsbysite, aes(x = Date, y = Num)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = lm)+
  ggtitle("Plant Richness") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Species")
plantsbysiteplot

plantsbysiteplotcolorsmooth <- ggplot(plantsbysite, aes(x = Date, y = Num, color = Site)) +
  geom_point(shape = 19, size = 2) +
  geom_smooth(method = lm) +
  ggtitle("Plant Richness") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Species")
plantsbysiteplotcolorsmooth

plantsbysiteplotcolorline <- ggplot(plantsbysite, aes(x = Date, y = Num, color = Site)) +
  geom_point(shape = 19, size = 2) +
  geom_line() +
  ggtitle("Plant Richness") +
  theme(plot.title = element_text(size = 20, face = "bold", margin = margin(10, 0, 10, 0))) +
  labs(x = "Date", y = "Number of Species")
plantsbysiteplotcolorline

#Mean bees and mean plants in one plot
bindbeesandplantsplot <- ggplot(bindbeesandplants, aes(x = Site, y = Mean, color = Taxa)) +
  geom_point(shape = 19, size = 2) +
  scale_color_discrete() +
  geom_smooth(method = lm) +
  ggtitle("Mean Number of Bees and Plant Species at Each Site") +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1, lineheight = 0.6, margin = margin(10, 0, 10, 0)))
bindbeesandplantsplot

# bees and plants in one plot
bindplot <- ggplot(bind, aes(x = TotalPlants, y = TotalBees, color = as.factor(Date))) +
  geom_point(shape = 19, size = 2) +
  scale_color_discrete() +
  #geom_smooth(method = lm) +
  ggtitle("Bee Abundance vs. Flowering Plant Richness by Date") +
  theme(plot.title = element_text(size = 20, face = "bold", vjust = 1, lineheight = 0.6, margin = margin(10, 0, 10, 0)))

#Bees and plants in one plot by period rather than date
ByPeriodplot <- ggplot(ByPeriod, aes(x = SPPlants, y = SPBees, color = (Sample.Period))) +
  geom_point(shape = 19, size = 3) +
  ggtitle("Bee Abundance vs. Flowering Plant Richness\nby Sample Period") +
  labs(x = "Flowering Plant Species Richness", y = "Bee Abundance") +
  theme(legend.title = element_text(color = "black", size = 12, face = NULL)) + 
  scale_color_discrete(name = "Sample Period", breaks = c("Early May", "Late May", "June", "July", "August")) +
  theme_bw()
ByPeriodplot

TotalsByPeriodplot <- ggplot(TotalsByPeriod, aes(x = SPPlants, y = SPBees, color = (SamplePeriod))) +
  geom_point(shape = 19, size = 3) +
  geom_line() +
  geom_abline() +
  ggtitle("Bee Abundance vs. Flowering Plant Richness\nby Sample Period") +
  labs(x = "Flowering Plant Species Richness", y = "Bee Abundance") +
  theme(legend.title = element_text(color = "black", size = 12, face = NULL)) + 
  scale_color_discrete(name = "Sample Period", breaks = c("EarlyMay", "LateMay", "June", "July", "August")) +
  theme_bw()
TotalsByPeriodplot

#Plot each site alone
Plunkettplot <- ggplot(Plunkett, aes(x = Date, y = Num)) +
  geom_point(shape = 19, size = 2) +
  geom_line() +
  labs(x = "Date", y = "Number of Individuals")
