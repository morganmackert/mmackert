#-------------------------------------------------------------------#
#             Sampling Effort + Trap Type ~ Bee Species             #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Research Question: How many sampling events are required to reach bee species saturation?

#Objectives:
#Create model(s) to explore relationship between sampling effort and the number of bee species collected as a result
#Use created model(s) to visualize the relationship graphically

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project")

#Load libraries
library(dplyr)
library(lubridate)
library(tidyr)

#Read in data
BeeIDs1234 <- read.csv("Data/Bees/Bee IDs.csv")
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet

#Remove unnecessary columns
BeeIDs1234 <- BeeIDs1234[-c(10:29)]

#Use lubridate to allow R to recognize the dates
BeeIDs1234$Date <- mdy(BeeIDs1234$Date)
#One date will fail to parse, which is "Target 2016." Don't worry about it.

#Remove "Wasp" entries
BeeIDs1234 <- BeeIDs1234 %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Wasp")

#Change Trap values from "Non-Target" to "NT" and "Emergence Trap" to "Emergence" to be consistent
BeeIDs1234$Trap[BeeIDs1234$Trap == "Non-Target"] <- "NT"
BeeIDs1234$Trap[BeeIDs1234$Trap == "Emergence Trap"] <- "Emergence"

BeeIDs1234$Date <- as.factor(BeeIDs1234$Date)
BeeIDs1234$Site <- as.factor(BeeIDs1234$Site)

#Create a new variable for number of samples taken
BeeIDs1234 <- BeeIDs1234 %>%
  group_by_(.dots = c("Date", "Site")) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

mtcars %>% 
  group_by_(.dots=c("mpg","hp","wt")) %>% 
  summarize(x=mean(gear))


#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Plunkett samples
PL1234 <- BeeIDs1234 %>%
  filter(Site == "Plunkett")

#Subset PL1234 to include only Plunkett samples collected with blue vane traps
PL1234BV <- PL1234 %>%
  filter(Trap == "Blue vane")

#Create a table showing the number of unique species collected during each sample
PL1234BVcount <-  PL1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL1234BVtable <- PL1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL1234BVtablewide <- spread(PL1234BVtable, Binomial, n)

#Change row names to assigned sample number
PL1234BVtablewide <- PL1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234BVtablewide[is.na(PL1234BVtablewide)] <- 0

#Convert to numeric
PL1234BVtablewide[] <- lapply(PL1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for Plunkett Blue Vane
rarecurve(PL1234BVtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected with bee bowls
PL1234BB <- PL1234 %>%
  filter(Trap == "Bowls")

#Create a table showing the number of species collected during each sample
PL1234BBcount <-  PL1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL1234BBtable <- PL1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL1234BBtablewide <- spread(PL1234BBtable, Binomial, n)

#Fill NAs with 0
PL1234BBtablewide[is.na(PL1234BBtablewide)] <- 0

#Convert to numeric
PL1234BBtablewide[] <- lapply(PL1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for Plunkett Blue Vane
rarecurve(PL1234BBtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected via non-targeted sweeping
PL1234NT <- PL1234 %>%
  filter(Trap == "NT")

#Create a table showing the number of species collected during each sample
PL1234NTcount <-  PL1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL1234NTtable <- PL1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL1234NTtablewide <- spread(PL1234NTtable, Binomial, n)

#Fill NAs with 0
PL1234NTtablewide[is.na(PL1234NTtablewide)] <- 0

#Convert to numeric
PL1234NTtablewide[] <- lapply(PL1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for Plunkett Blue Vane
rarecurve(PL1234NTtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected by emergence traps
PL1234ET <- PL1234 %>%
  filter(Trap == "Emergence")

#Create a table showing the number of species collected during each sample
PL1234ETcount <-  PL1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL1234ETtable <- PL1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL1234ETtablewide <- spread(PL1234ETtable, Binomial, n)

#Fill NAs with 0
PL1234ETtablewide[is.na(PL1234ETtablewide)] <- 0

#Convert to numeric
PL1234ETtablewide[] <- lapply(PL1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for Plunkett Blue Vane
rarecurve(PL1234ETtablewide)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Bowman samples
BO123 <- Fulldata %>%
  filter(Site == "Bowman") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
BO123$Sample <- 1:14

#Site and Year columns no longer necessary
BO123 <- BO123[-c(1:2)]

#Change row names to assigned sample number
BO123 <- BO123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Bowman
rarecurve(BO123)

#-------------------------------------------------------------------#
#                            Kaldenberg                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Kaldenberg samples
KA123 <- Fulldata %>%
  filter(Site == "Kaldenberg") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
KA123$Sample <- 1:14

#Site and Year columns no longer necessary
KA123 <- KA123[-c(1:2)]

#Change row names to assigned sample number
KA123 <- KA123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Kaldenberg
rarecurve(KA123)

#-------------------------------------------------------------------#
#                             McClellan                             #
#-------------------------------------------------------------------#
#Subset Fulldata to include only McClellan samples
MC123 <- Fulldata %>%
  filter(Site == "McClellan") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
MC123$Sample <- 1:14

#Site and Year columns no longer necessary
MC123 <- MC123[-c(1:2)]

#Change row names to assigned sample number
MC123 <- MC123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for McClellan
rarecurve(MC123)

#-------------------------------------------------------------------#
#                              Sloan                                #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Sloan samples
SL123 <- Fulldata %>%
  filter(Site == "Sloan") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
SL123$Sample <- 1:14

#Site and Year columns no longer necessary
SL123 <- SL123[-c(1:2)]

#Change row names to assigned sample number
SL123 <- SL123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Sloan
rarecurve(SL123)

#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Sheller samples
SH123 <- Fulldata %>%
  filter(Site == "Sheller") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
SH123$Sample <- 1:14

#Site and Year columns no longer necessary
SH123 <- SH123[-c(1:2)]

#Change row names to assigned sample number
SH123 <- SH123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Sheller
rarecurve(SH123)

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Cretsinger samples
CR123 <- Fulldata %>%
  filter(Site == "Cretsinger") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
CR123$Sample <- 1:14

#Site and Year columns no longer necessary
CR123 <- CR123[-c(1:2)]

#Change row names to assigned sample number
CR123 <- CR123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Cretsinger
rarecurve(CR123)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Subset Fulldata to include only Peckumn samples
PE123 <- Fulldata %>%
  filter(Site == "Peckumn") %>%
  filter(Year < "4")

#Assign a number for each sample and make it the row name
PE123$Sample <- 1:10

#Site and Year columns no longer necessary
PE123 <- PE123[-c(1:2)]

#Change row names to assigned sample number
PE123 <- PE123 %>%
  remove_rownames %>%
  column_to_rownames("Sample")

#Create rarefaction curve for Peckumn
rarecurve(PE123)






