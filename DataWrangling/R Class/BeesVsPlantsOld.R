########################################################
################   BEES VS. PLANTS    ##################
########################################################

#Research question: How do floral resources influence bee abundance?

#Set working directory
setwd("~ISU/Semester 3/R/mmackert/DataWrangling") #???

#Load libraries
library(lubridate)
library(lme4)
library(ggplot2)
library(tidyr)

#Read in files
bees <- read.csv("Data/bees/raw/2016Bees.csv", header = T)
plants <- read.csv("Data/plants/2016Total.csv", header = T)

simplebees <- read.csv("Data/bees/working/simplebees.csv", header =T)
simpleplants <- read.csv("Data/plants/simpleplants.csv", header = T)

View(simplebees)
View(simpleplants)

simpbees <- subset(bees[,c(1:2,4)])
simpbees2 <- aggregate(Bees ~ Site, data = simpbees, FUN = sum)
simpbees2[1,1] <- "Total"

#Forget all of this
#Using "bees" to get total number of bees for the entire season
solobees <- bees[,-(5:13)]
solobees

solobees <- solobees[-124,]
solobees

totalbees <- sum(solobees$Bees)
totalbees #"L" at the end of value?

#Fix dates in "bees" and "plants"
solobees$Date <- mdy(solobees$Date)
solobees
plants$Date <- mdy(plants$Date)
plants

simplebees$Date <- mdy(simplebees$Date)
simplebees
simpleplants$Date <- mdy(simpleplants$Date)
simpleplants

#Don't need this either
#Using "plants" to get total number of flowering species, minus "No blooms", for each site at each date. (Thank you, Phil Dixon.)
nplsp <- with(plants, table(Species))
nplsp

nplsp <- with(plants, tapply(X..Cover, Species, mean))
nplsp

nplsp <- with(plants, tapply(X..Cover, list(Date, Site), mean))
nplsp

nplsp <- with(plants, tapply(Species, list(Date, Site), function(x) { length(unique(x))}) )
nplsp

nplsp <- with(plants, tapply(Species, list(Date, Site), function(x) { length(unique(x[x !='No blooms']))}) )
nplsp

simplebees$Taxa <- "Bees"
simpleplants$Taxa <- "Plants"

#Use "cbind" to bind "simplebees" and "simpleplants" together

names(simplebees) <- c("Date", "Site", "Num", "Taxa")
names(simpleplants) <- c("Date", "Site", "Num", "Taxa")

otherbees <- simplebees
otherbees2 <- with(otherbees, aggregate(Num ~ Date, FUN = mean))
beesSD <- with(otherbees, aggregate(Num ~ Date, FUN = sd))
otherbees2$Taxa <- "Bees"
otherbees2$SD <- beesSD$Num

otherplants <- simpleplants
otherplants2 <- with(otherplants, aggregate(Num ~ Date, FUN = mean))
plantsSD <- with(otherplants, aggregate(Num ~ Date, FUN = sd))
otherplants2$Taxa <- "Plants"
otherplants2$SD <- plantsSD$Num

otherbind <- rbind(otherbees2, otherplants2)

par(mfrow = c(1,2))
with(otherbees2, plot(Num ~ Date, ylab = "Bee Abundance"));
with(otherbees2, abline(lm(Num~Date)))
with(otherplants2, plot(Num ~ Date, ylab = "Plant Richness"));
with(otherplants2, abline(lm(Num~Date)))


ggplot(otherbees2, aes(x = Date, y = Num, fill = Taxa)) + geom_line() + geom_point(shape=1)
geom_errorbar(aes(ymin = Num-SD, ymax = Num+SD))

ggplot(otherplants2, aes(x = Date, y = Num, fill = Taxa)) + 
  geom_line() + 
  geom_point(shape = 1) +
  geom_errorbar(aes(ymin = Num-SD, ymax = Num+SD))


ggplot(otherbind, aes(x = Date, y = Num, fill = Taxa)) + geom_line() + geom_point(shape=1)
  geom_errorbar(aes(ymin = Num-SD, ymax = Num+SD))

# Delete "#" for misc not immediately useful

#bind2 <- aggregate(Num ~ Date + Taxa, data = otherbind, FUN = sum)
#tool <- cbind(otherbees2$SD, otherplants2$SD)
#bind2$SD <- "SD"
#bind2$SD <- otherbees2$SD[match(bind2$Num, otherbees2$Num)]

#General linear model ######???????????
beesvsplants <- glm(formula = bind$TotalBees ~ bind$TotalPlants, family = poisson)

summary(beesvsplants)
plot(beesvsplants)







#Use ggplot2 to make pretty figures
#Change "shape=#" to make different points
ggplot(bind, aes(x = TotalBees, y = TotalPlants)) + geom_point(shape=1) + geom_smooth(method=lm) #This one is points with best fit line only
ggplot(bind, aes(x = TotalBees, y = TotalPlants)) + geom_line() + geom_point(shape=1) + geom_smooth(method=lm) #This one is points with connecting line between AND best fit line


ggplot(bind2, aes(x = Date, y = Num, fill = Taxa)) + geom_line() + geom_point(shape=1) + geom_smooth(method=lm)
