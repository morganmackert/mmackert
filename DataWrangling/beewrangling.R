#####################################
############### BEES ################
#####################################

#Set working directory
setwd("~/ISU/Project/mmackert")

#Read in data files
bees <- read.csv("Data/bees/working/2016Bees.csv", header = T)

#View data
View(bees)

#Load libraries
library(lubridate)
library(dplyr)

#Fix dates in "bees"
bees$Date <- mdy(bees$Date)

#Eliminate last row (totals)
bees <- bees[-c(124),]

#View data to check date/data changes
View(bees)


#Subset "bees" to eliminate extraneous data; only need "Date", "Site", and "Bees".
simplebees <- subset(bees[,c(1:2,4)])

#View new data frame
View(simplebees)
#Looks good

#Not worrying about trap effects just yet, so combine by date and site.
groupedbees <- bees %>% group_by(Site, Date)

#Summarize and view grouped bee data frame
summarybees <- groupedbees %>% summarise(total.bees = sum(Bees))





