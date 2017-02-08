##############################################################
################   BEES VS. PLANTS MUNGING  ##################
##############################################################

#Research question: How do floral resources influence bee abundance?

#Load libraries
library(ggplot2)
library(lubridate)
library(lme4)
library(tidyr)

#Read in files
setwd("C:/Users/Morgan/Documents/ISU/Semester 3/R/mmackert/Data")
bees <- read.csv("bees/working/2016Bees.csv", header = T)
plants <- read.csv("plants/raw/2016Total.csv", header = T)

#Fix dates in "bees" and "plants"
bees$Date <- mdy(bees$Date)
View(bees)

plants$Date <- mdy(plants$Date)
View(plants)

#Subset "bees" to eliminate extraenous data; need only "Date", "Site", and "Bees".
simpbees <- subset(bees[,c(1:2,4)])
simpbees2 <- aggregate(Bees ~ Site, data = simpbees, FUN = sum)
simpbees2[1,1] <- "Total"
#Couldn't figure out how to aggregate by two fields ("Site" and "Date") simultaneously. Cheated and created a new Excel file with the appropriate bee information. Will figure this out later.
#"plants" was also a mess I wasn't sure how to tackle. Cheated once more and created a new Excel file with the appropriate plant information. Will figure this out later.

#Read in simpler files
simplebees <- read.csv("bees/working/simplebees.csv", header =T)
simplebees$Date <- mdy(simplebees$Date)
View(simplebees)

simpleplants <- read.csv("plants/working/simpleplants.csv", header = T)
simpleplants$Date <- mdy(simpleplants$Date)
View(simpleplants)

#Add column to "simplebees" and "simpleplants" denoting which group they belong to.
simplebees$Taxa <- "Bees"
simpleplants$Taxa <- "Plants"

#Name columns appropriately
names(simplebees) <- c("Date", "Site", "TotalBees", "Taxa")
names(simpleplants) <- c("Date", "Site", "TotalPlants", "Taxa")

#Subset for each site
Plunkett <- subset(simplebees, simplebees$Site == "Plunkett")
Bowman <-  subset(simplebees, simplebees$Site == "Bowman")
Kaldenberg <- subset(simplebees, simplebees$Site == "Kaldenberg")
McClellan <-subset(simplebees, simplebees$Site == "McClellan")
Sheller <- subset(simplebees, simplebees$Site == "Sheller")
Sloan <- subset(simplebees, simplebees$Site == "Sloan")
Cretsinger <- subset(simplebees, simplebees$Site == "Cretsinger")
Peckumn <- subset(simplebees, simplebees$Site == "Peckumn")

#Aggregate by "Site" and calculate mean/sd number of bees collected at each site
beesbysite <- simplebees
beesbysitemean <- with(beesbysite, aggregate(Num ~ Site, FUN = mean))
beesbysiteSD <- with(beesbysite, aggregate(Num ~ Site, FUN = sd))

#Combine the two bee data frames (mean/sd), delete extra columns, and rename them.
beesbysitemeanSD <- cbind(beesbysitemean, beesbysiteSD)
beesbysitemeanSD <- beesbysitemeanSD[,-3]
beesbysitemeanSD$Taxa <- "Bees"
names(beesbysitemeanSD) <- c("Site", "Mean", "SD", "Taxa")
View(beesbysitemeanSD)

#Aggregate by "Site" and calculate mean/sd number of plant species found at each site.
plantsbysite <- simpleplants
plantsbysitemean <- with(plantsbysite, aggregate(TotalPlants ~ Site, FUN = mean))
plantsbysiteSD <- with(plantsbysite, aggregate(TotalPlants ~ Site, FUN = sd))

#Combine the two plant data frames (mean/sd), delete extra columns, and rename them.
plantsbysitemeanSD <- cbind(plantsbysitemean, plantsbysiteSD)
plantsbysitemeanSD <- plantsbysitemeanSD[,-3]
plantsbysitemeanSD$Taxa <- "Plants"
names(plantsbysitemeanSD) <- c("Site", "Mean", "SD", "Taxa")

#Use "rbind" to combine "beesbysitemeanSD" and "plantsbysitemeanSD"
bindbeesandplants <- rbind(beesbysitemeanSD, plantsbysitemeanSD)
write.csv(bindbeesandplants, "BindBeesandPlants.csv")

#Left with one data frame with all necessary information. Woo! Move to BeesVsPlantsAnalysisScripts.
