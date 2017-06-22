#####################################################################
#                            2016 WEATHER                           #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data/Weather")

#Load libraries
library(weathermetrics)
library(measurements)
library(plyr)

#-------------------------------------------------------------------#
#                              Plunkett                             #
#-------------------------------------------------------------------#
#Read in data
W16PL <- read.csv("Weather2016Plunkett.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16PL$Average.C <- fahrenheit.to.celsius(W16PL$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16PLACM <- mean(W16PL$Average.C)
W16PLACSD <- sd(W16PL$Average.C)

#Covert Precip.in from inches to millimeters
W16PL$Precip.mm <- conv_unit(W16PL$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16PLPMM <- mean(W16PL$Precip.mm)
W16PLPMSD <- sd(W16PL$Precip.mm)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Read in data
W16BO <- read.csv("Weather2016Bowman.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16BO$Average.C <- fahrenheit.to.celsius(W16BO$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16BOACM <- mean(W16BO$Average.C)
W16BOACSD <- sd(W16BO$Average.C)

#Covert Precip.in from inches to millimeters
W16BO$Precip.mm <- conv_unit(W16BO$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16BOPMM <- mean(W16BO$Precip.mm)
W16BOPMSD <- sd(W16BO$Precip.mm)

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Read in data
W16KA <- read.csv("Weather2016Kaldenberg.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16KA$Average.C <- fahrenheit.to.celsius(W16KA$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16KAACM <- mean(W16KA$Average.C)
W16KAACSD <- sd(W16KA$Average.C)

#Covert Precip.in from inches to millimeters.
W16KA$Precip.mm <- conv_unit(W16KA$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16KAPMM <- mean(W16KA$Precip.mm)
W16KAPMSD <- sd(W16KA$Precip.mm)

#-------------------------------------------------------------------#
#                             McClellan                             #
#-------------------------------------------------------------------#
#Read in data
W16MC <- read.csv("Weather2016McClellan.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16MC$Average.C <- fahrenheit.to.celsius(W16MC$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16MCACM <- mean(W16MC$Average.C)
W16MCACSD <- sd(W16MC$Average.C)

#Covert Precip.in from inches to millimeters.
W16MC$Precip.mm <- conv_unit(W16MC$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16MCPMM <- mean(W16MC$Precip.mm)
W16MCPMSD <- sd(W16MC$Precip.mm)

#-------------------------------------------------------------------#
#                              Sloan                                #
#-------------------------------------------------------------------#
#Read in data
W16SL <- read.csv("Weather2016Sloan.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16SL$Average.C <- fahrenheit.to.celsius(W16SL$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16SLACM <- mean(W16SL$Average.C)
W16SLACSD <- sd(W16SL$Average.C)

#Covert Precip.in from inches to millimeters.
W16SL$Precip.mm <- conv_unit(W16SL$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16SLPMM <- mean(W16SL$Precip.mm)
W16SLPMSD <- sd(W16SL$Precip.mm)

#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Read in data
W16SH <- read.csv("Weather2016Sheller.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16SH$Average.C <- fahrenheit.to.celsius(W16SH$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16SHACM <- mean(W16SH$Average.C)
W16SHACSD <- sd(W16SH$Average.C)

#Covert Precip.in from inches to millimeters.
W16SH$Precip.mm <- conv_unit(W16SH$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16SHPMM <- mean(W16SH$Precip.mm)
W16SHPMSD <- sd(W16SH$Precip.mm)

#-------------------------------------------------------------------#
#                            Cretsinger                             #
#-------------------------------------------------------------------#
#Read in data
W16CR <- read.csv("Weather2016Cretsinger.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16CR$Average.C <- fahrenheit.to.celsius(W16CR$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16CRACM <- mean(W16CR$Average.C)
W16CRACSD <- sd(W16CR$Average.C)

#Covert Precip.in from inches to millimeters.
W16CR$Precip.mm <- conv_unit(W16CR$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16CRPMM <- mean(W16CR$Precip.mm)
W16CRPMSD <- sd(W16CR$Precip.mm)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Read in data
W16PE <- read.csv("Weather2016Peckumn.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16PE$Average.C <- fahrenheit.to.celsius(W16PE$Average.F, round = 2)

#Calculate mean and standard deviation of Average.C
W16PEACM <- mean(W16PE$Average.C)
W16PEACSD <- sd(W16PE$Average.C)

#Covert Precip.in from inches to millimeters.
W16PE$Precip.mm <- conv_unit(W16PE$Precip.in, "inch", "mm")

#Calculate mean and standard deviation of Precip.mm
W16PEPMM <- mean(W16PE$Precip.mm)
W16PEPMSD <- sd(W16PE$Precip.mm)

#-------------------------------------------------------------------#
#                                 All                               #
#-------------------------------------------------------------------#
#Create new vectors to compile the data
#Site names vector
W16AllNames <- c("Plunkett", "Bowman", "Kaldenberg", "McClellan", "Sloan", "Sheller", "Cretsinger", "Peckumn")
#Average temperature in Celcius vector
W16AllACM <- c(W16PLACM, W16BOACM, W16KAACM, W16MCACM, W16SLACM, W16SHACM, W16CRACM, W16PEACM)
#Average precipitation in millimeters vector
W16AllPMM <- c(W16PLPMM, W16BOPMM, W16KAPMM, W16MCPMM, W16SLPMM, W16SHPMM, W16CRPMM, W16PEPMM)
#Standard deviation of temperature in Celcius vector
W16AllACSD <- c(W16PLACSD, W16BOACSD, W16KAACSD, W16MCACSD, W16SLACSD, W16SHACSD, W16CRACSD, W16PEACSD)
#Standard deviation of precipitation in millimeters vector
W16AllPMSD <- c(W16PLPMSD, W16BOPMSD, W16KAPMSD, W16MCPMSD, W16SLPMSD, W16SHPMSD, W16CRPMSD, W16PEPMSD)

#Combine the three new vectors into a single dataframe
W16All <- data.frame(W16AllNames, W16AllACM, W16AllACSD, W16AllPMM, W16AllPMSD)

#Rename the columns
W16All <- rename(W16All, c("W16AllNames" = "Site Name", "W16AllACM" = "Average Temperature (°C)", "W16AllACSD" = "Temperature Standard Deviation (°C)", "W16AllPMM" = "Average Precipitation (mm)", "W16AllPMSD" = "Precipitation Standard Deviation (mm)"))
