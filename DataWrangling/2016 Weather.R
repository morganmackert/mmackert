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

#Calculate mean of Average.C
W16PLAC <- mean(W16PL$Average.C)

#Covert Precip.in from inches to millimeters.
W16PL$Precip.mm <- conv_unit(W16PL$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16PLPM <- mean(W16PL$Precip.mm)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Read in data
W16BO <- read.csv("Weather2016Bowman.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16BO$Average.C <- fahrenheit.to.celsius(W16BO$Average.F, round = 2)

#Calculate mean of Average.C
W16BOAC <- mean(W16BO$Average.C)

#Covert Precip.in from inches to millimeters.
W16BO$Precip.mm <- conv_unit(W16BO$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16BOPM <- mean(W16BO$Precip.mm)

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Read in data
W16KA <- read.csv("Weather2016Kaldenberg.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16KA$Average.C <- fahrenheit.to.celsius(W16KA$Average.F, round = 2)

#Calculate mean of Average.C
W16KAAC <- mean(W16KA$Average.C)

#Covert Precip.in from inches to millimeters.
W16KA$Precip.mm <- conv_unit(W16KA$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16KAPM <- mean(W16KA$Precip.mm)

#-------------------------------------------------------------------#
#                             McClellan                             #
#-------------------------------------------------------------------#
#Read in data
W16MC <- read.csv("Weather2016McClellan.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16MC$Average.C <- fahrenheit.to.celsius(W16MC$Average.F, round = 2)

#Calculate mean of Average.C
W16MCAC <- mean(W16MC$Average.C)

#Covert Precip.in from inches to millimeters.
W16MC$Precip.mm <- conv_unit(W16MC$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16MCPM <- mean(W16MC$Precip.mm)

#-------------------------------------------------------------------#
#                              Sloan                                #
#-------------------------------------------------------------------#
#Read in data
W16SL <- read.csv("Weather2016Sloan.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16SL$Average.C <- fahrenheit.to.celsius(W16SL$Average.F, round = 2)

#Calculate mean of Average.C
W16SLAC <- mean(W16SL$Average.C)

#Covert Precip.in from inches to millimeters.
W16SL$Precip.mm <- conv_unit(W16SL$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16SLPM <- mean(W16SL$Precip.mm)

#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Read in data
W16SH <- read.csv("Weather2016Sheller.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16SH$Average.C <- fahrenheit.to.celsius(W16SH$Average.F, round = 2)

#Calculate mean of Average.C
W16SHAC <- mean(W16SH$Average.C)

#Covert Precip.in from inches to millimeters.
W16SH$Precip.mm <- conv_unit(W16SH$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16SHPM <- mean(W16SH$Precip.mm)

#-------------------------------------------------------------------#
#                            Cretsinger                             #
#-------------------------------------------------------------------#
#Read in data
W16CR <- read.csv("Weather2016Cretsinger.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16CR$Average.C <- fahrenheit.to.celsius(W16CR$Average.F, round = 2)

#Calculate mean of Average.C
W16CRAC <- mean(W16CR$Average.C)

#Covert Precip.in from inches to millimeters.
W16CR$Precip.mm <- conv_unit(W16CR$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16CRPM <- mean(W16CR$Precip.mm)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Read in data
W16PE <- read.csv("Weather2016Peckumn.csv")

#Convert Average.F from Fahrenheit to Celcius and place values into a new column within the dataframe.
W16PE$Average.C <- fahrenheit.to.celsius(W16PE$Average.F, round = 2)

#Calculate mean of Average.C
W16PEAC <- mean(W16PE$Average.C)

#Covert Precip.in from inches to millimeters.
W16PE$Precip.mm <- conv_unit(W16PE$Precip.in, "inch", "mm")

#Calculate mean of Precip.mm
W16PEPM <- mean(W16PE$Precip.mm)

#-------------------------------------------------------------------#
#                                 All                               #
#-------------------------------------------------------------------#
#Create three new vectors to compile the data
#Site names vector
W16AllNames <- c("Plunkett", "Bowman", "Kaldenberg", "McClellan", "Sloan", "Sheller", "Cretsinger", "Peckumn")

#Average temperature in Celcius vector
W16AllAC <- c(W16PLAC, W16BOAC, W16KAAC, W16MCAC, W16SLAC, W16SHAC, W16CRAC, W16PEAC)

#Average precipitation in millimeters vector
W16AllPM <- c(W16PLPM, W16BOPM, W16KAPM, W16MCPM, W16SLPM, W16SHPM, W16CRPM, W16PEPM)

#Combine the three new vectors into a single dataframe
W16All <- data.frame(W16AllNames, W16AllAC, W16AllPM)

#Rename the columns
W16All <- rename(W16All, c("W16AllNames" = "Site Name", "W16AllAC" = "Average Temperature (°C)", "W16AllPM" = "Average Precipitation (mm)"))

sd(W16AllAC)
