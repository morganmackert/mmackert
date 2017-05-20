#####################################################################
#        CHI SQUARE AND INVERSE SIMPSON'S DIVERSITY INDEX           #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Previous Data/Data Files")

#Load libraries
library(MASS)
library(vegan)

#-------------------------------------------------------------------#
#                    Pearson's Chi-Square Test                      #
#-------------------------------------------------------------------#

#Read in data
CPCS <- read.csv("Cleptoparasite_Chi_Square.csv")
#####What do these columns (HP and HNP) and corresponding values mean?
#####Host Present and Host Not Present

#Create new vectors
col1 <- c(1,4,1,5,1,1,1,4,2,2,3,1,1,1,1,1,2,1,1,1,1,1,1)
col2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#####What do these values mean? Why create them?

#Bind the two new vectors together
probs <- cbind(col1,col2)

#Perform Chi-Square test on orignial dataset
chisq.test(CPCS)
#####If performing test on original dataset, why make probs?
#####Throws warning: Chi-squared approximation may be incorrect.

GCCS <- read.csv("Guild_Contingency_Table.csv")
#####What do the numbered rows mean?
#####Numbers (1-7) are guilds
#####1: Solitary ground nesters; 2: Social ground nesters, 3: Honeybees, 4: Bumblebees, 5: Cavity nesters, 6: Cleptoparasites, 7: Social parasites
chisq.test(GCCS)
#####Throws warning: Chi-squared approximation may be incorrect.

#-------------------------------------------------------------------#
#                Inverse Simpson's Diversity Index                  #
#-------------------------------------------------------------------#

#Read in data
dat <- read.csv("Clustering_NMDS_Bees_Year.csv")

#Move the variable that contains year, treatment, etc. to rownames
row.names(dat) <- dat[,1]

#Remove the label column
dat <- dat[,-1]

#Provides Inverse Simpson's Diversity Indices
ISDI <- diversity(dat, index = "invsimpson")
ISDI
#####Why do we use the inverse instead of the regular?
#####Inverse means number of species. Double check this.

#Provides Simpson's Diversity Indices
SDI <- diversity(dat, index = "simpson")
SDI