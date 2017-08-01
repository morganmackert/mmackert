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
#####What do the rows mean?

#Create new vectors
col1 <- c(1,4,1,5,1,1,1,4,2,2,3,1,1,1,1,1,2,1,1,1,1,1,1)
col2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#####What do these values mean? Why create them?
#####If supposed to be the first column of CPCS, values are incorrect. 1s substituted for all 0s in col1. Why???

#Bind the two new vectors together
probs <- cbind(col1,col2)

#Perform Chi-Square test on orignial dataset
chisq.test(CPCS)
#####If performing test on original dataset, why make "probs"?
#####Throws warning: Chi-squared approximation may be incorrect.

#More to be gleaned from this test! Perform test and assign a name
XsqCPCS <- chisq.test(CPCS)
#Observed counts, will be the same values as the input matrix
XsqCPCS$observed
#Expected counts under null hypothesis
XsqCPCS$expected
#Pearson residuals
XsqCPCS$residuals
#Standardized residuals
XsqCPCS$stdres
#P-value; given in output, but can extract separately too; p-value = 0.018
XsqCPCS$p.value
#Simulating p-value, computes p-values by Monte Carlo simulation; p-value = 0.073
XsqpCPCS <- chisq.test(CPCS, simulate.p.value = TRUE, B = 10000)
XsqpCPCS$p.value

#####What is Monte Carlo?
#####"Monte Carlo simulation performs risk analysis by building models of possible results by substituting a range of values—a probability distribution—for any factor that has inherent uncertainty. It then calculates results over and over, each time using a different set of random values from the probability functions."
#http://www.palisade.com/risk/monte_carlo_simulation.asp
#http://quantmleap.com/blog/2009/10/monte-carlo-simulation-for-dummies/

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
#####Inverse (1-D) is more intuitive; a higher number indicates greater diversity. Simpson's is the opposite, where a smaller number indicates lower diversity.

#Provides Simpson's Diversity Indices
SDI <- diversity(dat, index = "simpson")
SDI
