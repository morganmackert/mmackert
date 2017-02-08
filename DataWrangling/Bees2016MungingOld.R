###########################################
########### 2016 Bees Munging #############
###########################################

#Research Question:

#Do webs on Guam capture more prey than webs on Saipan? We want to know whether the number of prey captured varies by island (and thus, by bird presence/absence). 
#Model: preynum~island, family=poisson

#Load libraries
library(lubridate)

setwd("~/ISU/Semester 3/R/mmackert/Data/bees/raw")
Bees2016 <- read.csv("2016Bees.csv")


#Fix date
Bees2016$Date <- mdy(Bees2016$Date)


#Change working directory
setwd("~/ISU/Semester 3/R/mmackert/Data/bees/working")

write.csv(Bees2016,"Bees2016.csv")