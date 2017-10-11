## AES Consulting
## Client: Morgan Mackert <mmackert@iastate.edu>
## PI: Mary Harris <maharris@iastate.edu>
## Department: EEOB
## Consultant: Katherine Goode <kgoode@iastate.edu>
## Purpose: Code for editing the plant coverage data to make some  
##          corrections and add some new variables
## Date created: 2017/09/27
## Last modified: 2017/10/3

# Load libraries
library(lubridate)
library(dplyr)

# Set working directory
setwd("~/Desktop/Consulting/Clients/Mackert, Morgan")

# Load in data set with plant coverage data from first 3 years
plant_coverage <- read.csv("Data/PlantCoverage_MMM.csv")

############################
## Creating new variables ##
############################

# Remove the ID column
plant_coverage <- plant_coverage %>%
  select(-X)
  
# Convert Date to a date variable using lubridate
plant_coverage$Date <- mdy(plant_coverage$Date)

# Extract the year from Date and create a new variable called Year
plant_coverage$Year <- year(plant_coverage$Date)

# Create a day of the year variable for graphing purposes
plant_coverage$DayofYear <- yday(plant_coverage$Date)

#####################################
## Correcting Mistakes in the Data ##
#####################################

# Save the incorrect sampling day variable
plant_coverage$Sampling_Day_wrong <- plant_coverage$Sampling_Day



# Create a data set with the first 12 values in the dataset which contain the 
# wrong sample days
plant_coverage_wrongvalues <- plant_coverage %>%
  filter(Year == 2016, DayofYear < 143)

# Correct the sampling day values
for(i in 1:length(plant_coverage_wrongvalues$Date)){
  ifelse(plant_coverage_wrongvalues$DayofYear[i] < 139, 
         plant_coverage_wrongvalues$Sampling_Day[i] <- 1,
         plant_coverage_wrongvalues$Sampling_Day[i] <- 2)  
}

# Create a data set with all of the correct values
plant_coverage_correctvalues <- plant_coverage %>%
  filter(Year != 2016 | DayofYear >= 143)

# Join the corrected values with whole dataset
plant_coverage <- rbind(plant_coverage_wrongvalues, plant_coverage_correctvalues)

###########################
## Export the Data Frame ##
###########################

# Write the data frame to a csv file
write.csv(plant_coverage, "Data/PlantCoverage_3year.csv", row.names = FALSE)
