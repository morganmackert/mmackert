########################################################
################   BEES VS. PLANTS    ##################
########################################################

#Research question: How do increased floral resources influence bee abundance?

#Model: totalbees ~ totalplants, family = Poisson

#Set working directory
setwd("~ISU/Semester 3/R/mmackert/DataWrangling") #???

#Read in files
bees <- read.csv("Data/bees/raw/2016Bees.csv", header=T)
plants <- read.csv("Data/plants/2016Total.csv", header=T)

#Load packages

#Using "bees" to get total number of bees for the entire season
solobees <- bees[,-12]
solobees

solobees <- bees[,-(5:13)]
solobees

solobees <- solobees[-124,]
solobees

totalbees <- sum(solobees$Bees)
totalbees

#Fix date
solobees$Date <- mdy(solobees$Date)

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

#General linear model
glm <- glm(totalbees ~ nplsp, family = poisson) #???