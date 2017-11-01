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
library(tibble)
library(vegan)

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

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
BeeIDs1234 %>%
  group_by(Site) %>%
  summarise()

#Same with "Trap"
BeeIDs1234 %>%
  group_by(Trap) %>%
  summarise()

#We find that site names are good to go, but trap names need some work!
BeeIDs1234$Trap[BeeIDs1234$Trap == "Non-Target"] <- "NT"
BeeIDs1234$Trap[BeeIDs1234$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs1234$Trap[BeeIDs1234$Trap == "Blue Vane"] <- "Blue vane"

#Create a new variable for number of samples taken
#BeeIDs1234 <- BeeIDs1234 %>%
  #group_by(Date) %>%
  #mutate(Sampling_Day = as.factor(dense_rank(Date)))

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Plunkett samples
PL1234 <- BeeIDs1234 %>%
  filter(Site == "Plunkett")

#Create a new variable for number of samples taken
PL1234 <- PL1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset PL1234 to include only Plunkett samples collected with blue vane traps
PL1234BV <- PL1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(PL1234BV$Date))

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

#Change PL1234BVtablewide to dataframe
PL1234BVtablewide <- as.data.frame(PL1234BVtablewide)

#Change row names to assigned sample number
PL1234BVtablewide <- PL1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234BVtablewide[is.na(PL1234BVtablewide)] <- 0

#Convert to numeric
PL1234BVtablewide[] <- lapply(PL1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PL1234BVtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected with bee bowls
PL1234BB <- PL1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(PL1234BB$Date))

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

#Change PL1234BBtablewide to dataframe
PL1234BBtablewide <- as.data.frame(PL1234BBtablewide)

#Change row names to assigned sample number
PL1234BBtablewide <- PL1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234BBtablewide[is.na(PL1234BBtablewide)] <- 0

#Convert to numeric
PL1234BBtablewide[] <- lapply(PL1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PL1234BBtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected via non-targeted sweeping
PL1234NT <- PL1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(PL1234NT$Date))

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

#Change PL1234NTtablewide to dataframe
PL1234NTtablewide <- as.data.frame(PL1234NTtablewide)

#Change row names to assigned sample number
PL1234NTtablewide <- PL1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234NTtablewide[is.na(PL1234NTtablewide)] <- 0

#Convert to numeric
PL1234NTtablewide[] <- lapply(PL1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PL1234NTtablewide)

#-------------------------------------------------------------------#
#                              PLUNKETT                             #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset PL1234 to include only Plunkett samples collected by emergence traps
PL1234ET <- PL1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 4.
length(unique(PL1234ET$Date))

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

#Change PL1234ETtablewide to dataframe
PL1234ETtablewide <- as.data.frame(PL1234ETtablewide)

#Change row names to assigned sample number
PL1234ETtablewide <- PL1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234ETtablewide[is.na(PL1234ETtablewide)] <- 0

#Convert to numeric
PL1234ETtablewide[] <- lapply(PL1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PL1234ETtablewide)

#-------------------------------------------------------------------#
#                               BOWMAN                              #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Bowman samples
BO1234 <- BeeIDs1234 %>%
  filter(Site == "Bowman")

#Create a new variable for number of samples taken
BO1234 <- BO1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset BO1234 to include only Bowman samples collected with blue vane traps
BO1234BV <- BO1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(BO1234BV$Date))

#Create a table showing the number of unique species collected during each sample
BO1234BVcount <-  BO1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO1234BVtable <- BO1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO1234BVtablewide <- spread(BO1234BVtable, Binomial, n)

#Change BO1234BVtablewide to dataframe
BO1234BVtablewide <- as.data.frame(BO1234BVtablewide)

#Change row names to assigned sample number
BO1234BVtablewide <- BO1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO1234BVtablewide[is.na(BO1234BVtablewide)] <- 0

#Convert to numeric
BO1234BVtablewide[] <- lapply(BO1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(BO1234BVtablewide)

#-------------------------------------------------------------------#
#                               BOWMAN                              #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset BO1234 to include only Bowman samples collected with bee bowls
BO1234BB <- BO1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(BO1234BB$Date))

#Create a table showing the number of species collected during each sample
BO1234BBcount <-  BO1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO1234BBtable <- BO1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO1234BBtablewide <- spread(BO1234BBtable, Binomial, n)

#Change BO1234BBtablewide to dataframe
BO1234BBtablewide <- as.data.frame(BO1234BBtablewide)

#Change row names to assigned sample number
BO1234BBtablewide <- BO1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO1234BBtablewide[is.na(BO1234BBtablewide)] <- 0

#Convert to numeric
BO1234BBtablewide[] <- lapply(BO1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for
rarecurve(BO1234BBtablewide)

#-------------------------------------------------------------------#
#                                BOWMAN                             #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset BO1234 to include only Bowman samples collected via non-targeted sweeping
BO1234NT <- BO1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 7.
length(unique(BO1234NT$Date))

#Create a table showing the number of species collected during each sample
BO1234NTcount <-  BO1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO1234NTtable <- BO1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO1234NTtablewide <- spread(BO1234NTtable, Binomial, n)

#Change BO1234NTtablewide to dataframe
BO1234NTtablewide <- as.data.frame(BO1234NTtablewide)

#Change row names to assigned sample number
BO1234NTtablewide <- BO1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO1234NTtablewide[is.na(BO1234NTtablewide)] <- 0

#Convert to numeric
BO1234NTtablewide[] <- lapply(BO1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(BO1234NTtablewide)

#-------------------------------------------------------------------#
#                               BOWMAN                              #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset BO1234 to include only Bowman samples collected by emergence traps
BO1234ET <- BO1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 2.
length(unique(BO1234ET$Date))

#Create a table showing the number of species collected during each sample
BO1234ETcount <-  BO1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO1234ETtable <- BO1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO1234ETtablewide <- spread(BO1234ETtable, Binomial, n)

#Change BO1234ETtablewide to dataframe
BO1234ETtablewide <- as.data.frame(BO1234ETtablewide)

#Change row names to assigned sample number
BO1234ETtablewide <- BO1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO1234ETtablewide[is.na(BO1234ETtablewide)] <- 0

#Convert to numeric
BO1234ETtablewide[] <- lapply(BO1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve for Bowman Blue Vane
rarecurve(BO1234ETtablewide)

#-------------------------------------------------------------------#
#                             KALDENBERG                            #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Kaldenberg samples
KA1234 <- BeeIDs1234 %>%
  filter(Site == "Kaldenberg")

#Create a new variable for number of samples taken
KA1234 <- KA1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset KA1234 to include only Kaldenberg samples collected with blue vane traps
KA1234BV <- KA1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(KA1234BV$Date))

#Create a table showing the number of unique species collected during each sample
KA1234BVcount <-  KA1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA1234BVtable <- KA1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA1234BVtablewide <- spread(KA1234BVtable, Binomial, n)

#Change KA1234BVtablewide to dataframe
KA1234BVtablewide <- as.data.frame(KA1234BVtablewide)

#Change row names to assigned sample number
KA1234BVtablewide <- KA1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA1234BVtablewide[is.na(KA1234BVtablewide)] <- 0

#Convert to numeric
KA1234BVtablewide[] <- lapply(KA1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(KA1234BVtablewide)

#-------------------------------------------------------------------#
#                             KALDENBERG                            #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset KA1234 to include only Kaldenberg samples collected with bee bowls
KA1234BB <- KA1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(KA1234BB$Date))

#Create a table showing the number of species collected during each sample
KA1234BBcount <-  KA1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA1234BBtable <- KA1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA1234BBtablewide <- spread(KA1234BBtable, Binomial, n)

#Change KA1234BBtablewide to dataframe
KA1234BBtablewide <- as.data.frame(KA1234BBtablewide)

#Change row names to assigned sample number
KA1234BBtablewide <- KA1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA1234BBtablewide[is.na(KA1234BBtablewide)] <- 0

#Convert to numeric
KA1234BBtablewide[] <- lapply(KA1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(KA1234BBtablewide)

#-------------------------------------------------------------------#
#                             KALDENBERG                            #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset KA1234 to include only Kaldenberg samples collected via non-targeted sweeping
KA1234NT <- KA1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 14.
length(unique(KA1234NT$Date))

#Create a table showing the number of species collected during each sample
KA1234NTcount <-  KA1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA1234NTtable <- KA1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA1234NTtablewide <- spread(KA1234NTtable, Binomial, n)

#Change KA1234NTtablewide to dataframe
KA1234NTtablewide <- as.data.frame(KA1234NTtablewide)

#Change row names to assigned sample number
KA1234NTtablewide <- KA1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA1234NTtablewide[is.na(KA1234NTtablewide)] <- 0

#Convert to numeric
KA1234NTtablewide[] <- lapply(KA1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(KA1234NTtablewide)

#-------------------------------------------------------------------#
#                             KALDENBERG                            #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset KA1234 to include only Kaldenberg samples collected by emergence traps
KA1234ET <- KA1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 2.
length(unique(KA1234ET$Date))

#Create a table showing the number of species collected during each sample
KA1234ETcount <-  KA1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA1234ETtable <- KA1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA1234ETtablewide <- spread(KA1234ETtable, Binomial, n)

#Change KA1234ETtablewide to dataframe
KA1234ETtablewide <- as.data.frame(KA1234ETtablewide)

#Change row names to assigned sample number
KA1234ETtablewide <- KA1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA1234ETtablewide[is.na(KA1234ETtablewide)] <- 0

#Convert to numeric
KA1234ETtablewide[] <- lapply(KA1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(KA1234ETtablewide)

#-------------------------------------------------------------------#
#                              MCCLELLAN                            #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only McClellan samples
MC1234 <- BeeIDs1234 %>%
  filter(Site == "McClellan")

#Create a new variable for number of samples taken
MC1234 <- MC1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset MC1234 to include only McClellan samples collected with blue vane traps
MC1234BV <- MC1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(MC1234BV$Date))

#Create a table showing the number of unique species collected during each sample
MC1234BVcount <-  MC1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC1234BVtable <- MC1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC1234BVtablewide <- spread(MC1234BVtable, Binomial, n)

#Change MC1234BVtablewide to dataframe
MC1234BVtablewide <- as.data.frame(MC1234BVtablewide)

#Change row names to assigned sample number
MC1234BVtablewide <- MC1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC1234BVtablewide[is.na(MC1234BVtablewide)] <- 0

#Convert to numeric
MC1234BVtablewide[] <- lapply(MC1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(MC1234BVtablewide)

#-------------------------------------------------------------------#
#                              MCCLELLAN                            #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset MC1234 to include only McClellan samples collected with bee bowls
MC1234BB <- MC1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(MC1234BB$Date))

#Create a table showing the number of species collected during each sample
MC1234BBcount <-  MC1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC1234BBtable <- MC1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC1234BBtablewide <- spread(MC1234BBtable, Binomial, n)

#Change MC1234BBtablewide to dataframe
MC1234BBtablewide <- as.data.frame(MC1234BBtablewide)

#Change row names to assigned sample number
MC1234BBtablewide <- MC1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC1234BBtablewide[is.na(MC1234BBtablewide)] <- 0

#Convert to numeric
MC1234BBtablewide[] <- lapply(MC1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(MC1234BBtablewide)

#-------------------------------------------------------------------#
#                             MCCLELLAN                            #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset MC1234 to include only McClellan samples collected via non-targeted sweeping
MC1234NT <- MC1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 12.
length(unique(MC1234NT$Date))

#Create a table showing the number of species collected during each sample
MC1234NTcount <-  MC1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC1234NTtable <- MC1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC1234NTtablewide <- spread(MC1234NTtable, Binomial, n)

#Change MC1234NTtablewide to dataframe
MC1234NTtablewide <- as.data.frame(MC1234NTtablewide)

#Change row names to assigned sample number
MC1234NTtablewide <- MC1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC1234NTtablewide[is.na(MC1234NTtablewide)] <- 0

#Convert to numeric
MC1234NTtablewide[] <- lapply(MC1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(MC1234NTtablewide)

#-------------------------------------------------------------------#
#                              MCCLELLAN                            #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset MC1234 to include only McClellan samples collected by emergence traps
MC1234ET <- MC1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 3.
length(unique(MC1234ET$Date))

#Create a table showing the number of species collected during each sample
MC1234ETcount <-  MC1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC1234ETtable <- MC1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC1234ETtablewide <- spread(MC1234ETtable, Binomial, n)

#Change MC1234ETtablewide to dataframe
MC1234ETtablewide <- as.data.frame(MC1234ETtablewide)

#Change row names to assigned sample number
MC1234ETtablewide <- MC1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC1234ETtablewide[is.na(MC1234ETtablewide)] <- 0

#Convert to numeric
MC1234ETtablewide[] <- lapply(MC1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(MC1234ETtablewide)

#-------------------------------------------------------------------#
#                                SLOAN                              #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Sloan samples
SL1234 <- BeeIDs1234 %>%
  filter(Site == "Sloan")

#Create a new variable for number of samples taken
SL1234 <- SL1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset SL1234 to include only Sloan samples collected with blue vane traps
SL1234BV <- SL1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SL1234BV$Date))

#Create a table showing the number of unique species collected during each sample
SL1234BVcount <-  SL1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL1234BVtable <- SL1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL1234BVtablewide <- spread(SL1234BVtable, Binomial, n)

#Change SL1234BVtablewide to dataframe
SL1234BVtablewide <- as.data.frame(SL1234BVtablewide)

#Change row names to assigned sample number
SL1234BVtablewide <- SL1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL1234BVtablewide[is.na(SL1234BVtablewide)] <- 0

#Convert to numeric
SL1234BVtablewide[] <- lapply(SL1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SL1234BVtablewide)

#-------------------------------------------------------------------#
#                                SLOAN                              #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset SL1234 to include only Sloan samples collected with bee bowls
SL1234BB <- SL1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SL1234BB$Date))

#Create a table showing the number of species collected during each sample
SL1234BBcount <-  SL1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL1234BBtable <- SL1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL1234BBtablewide <- spread(SL1234BBtable, Binomial, n)

#Change SL1234BBtablewide to dataframe
SL1234BBtablewide <- as.data.frame(SL1234BBtablewide)

#Change row names to assigned sample number
SL1234BBtablewide <- SL1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL1234BBtablewide[is.na(SL1234BBtablewide)] <- 0

#Convert to numeric
SL1234BBtablewide[] <- lapply(SL1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SL1234BBtablewide)

#-------------------------------------------------------------------#
#                                SLOAN                              #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset SL1234 to include only Sloan samples collected via non-targeted sweeping
SL1234NT <- SL1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 16.
length(unique(SL1234NT$Date))

#Create a table showing the number of species collected during each sample
SL1234NTcount <-  SL1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL1234NTtable <- SL1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL1234NTtablewide <- spread(SL1234NTtable, Binomial, n)

#Change SL1234NTtablewide to dataframe
SL1234NTtablewide <- as.data.frame(SL1234NTtablewide)

#Change row names to assigned sample number
SL1234NTtablewide <- SL1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL1234NTtablewide[is.na(SL1234NTtablewide)] <- 0

#Convert to numeric
SL1234NTtablewide[] <- lapply(SL1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SL1234NTtablewide)

#-------------------------------------------------------------------#
#                                SLOAN                              #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset SL1234 to include only Sloan samples collected by emergence traps
SL1234ET <- SL1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 2.
length(unique(SL1234ET$Date))

#Create a table showing the number of species collected during each sample
SL1234ETcount <-  SL1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL1234ETtable <- SL1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL1234ETtablewide <- spread(SL1234ETtable, Binomial, n)

#Change SL1234ETtablewide to dataframe
SL1234ETtablewide <- as.data.frame(SL1234ETtablewide)

#Change row names to assigned sample number
SL1234ETtablewide <- SL1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL1234ETtablewide[is.na(SL1234ETtablewide)] <- 0

#Convert to numeric
SL1234ETtablewide[] <- lapply(SL1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SL1234ETtablewide)

#-------------------------------------------------------------------#
#                               SHELLER                             #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Sheller samples
SH1234 <- BeeIDs1234 %>%
  filter(Site == "Sheller")

#Create a new variable for number of samples taken
SH1234 <- SH1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset SH1234 to include only Sheller samples collected with blue vane traps
SH1234BV <- SH1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SH1234BV$Date))

#Create a table showing the number of unique species collected during each sample
SH1234BVcount <-  SH1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH1234BVtable <- SH1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH1234BVtablewide <- spread(SH1234BVtable, Binomial, n)

#Change SH1234BVtablewide to dataframe
SH1234BVtablewide <- as.data.frame(SH1234BVtablewide)

#Change row names to assigned sample number
SH1234BVtablewide <- SH1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH1234BVtablewide[is.na(SH1234BVtablewide)] <- 0

#Convert to numeric
SH1234BVtablewide[] <- lapply(SH1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SH1234BVtablewide)

#-------------------------------------------------------------------#
#                               SHELLER                             #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset SH1234 to include only Sheller samples collected with bee bowls
SH1234BB <- SH1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(SH1234BB$Date))

#Create a table showing the number of species collected during each sample
SH1234BBcount <-  SH1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH1234BBtable <- SH1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH1234BBtablewide <- spread(SH1234BBtable, Binomial, n)

#Change SH1234BBtablewide to dataframe
SH1234BBtablewide <- as.data.frame(SH1234BBtablewide)

#Change row names to assigned sample number
SH1234BBtablewide <- SH1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH1234BBtablewide[is.na(SH1234BBtablewide)] <- 0

#Convert to numeric
SH1234BBtablewide[] <- lapply(SH1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SH1234BBtablewide)

#-------------------------------------------------------------------#
#                               SHELLER                             #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset SH1234 to include only Sheller samples collected via non-targeted sweeping
SH1234NT <- SH1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(SH1234NT$Date))

#Create a table showing the number of species collected during each sample
SH1234NTcount <-  SH1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH1234NTtable <- SH1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH1234NTtablewide <- spread(SH1234NTtable, Binomial, n)

#Change SH1234NTtablewide to dataframe
SH1234NTtablewide <- as.data.frame(SH1234NTtablewide)

#Change row names to assigned sample number
SH1234NTtablewide <- SH1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH1234NTtablewide[is.na(SH1234NTtablewide)] <- 0

#Convert to numeric
SH1234NTtablewide[] <- lapply(SH1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SH1234NTtablewide)

#-------------------------------------------------------------------#
#                               SHELLER                             #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset SH1234 to include only Sheller samples collected by emergence traps
SH1234ET <- SH1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 3.
length(unique(SH1234ET$Date))

#Create a table showing the number of species collected during each sample
SH1234ETcount <-  SH1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH1234ETtable <- SH1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH1234ETtablewide <- spread(SH1234ETtable, Binomial, n)

#Change SH1234ETtablewide to dataframe
SH1234ETtablewide <- as.data.frame(SH1234ETtablewide)

#Change row names to assigned sample number
SH1234ETtablewide <- SH1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH1234ETtablewide[is.na(SH1234ETtablewide)] <- 0

#Convert to numeric
SH1234ETtablewide[] <- lapply(SH1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(SH1234ETtablewide)

#-------------------------------------------------------------------#
#                              CRETSINGER                           #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Cretsinger samples
CR1234 <- BeeIDs1234 %>%
  filter(Site == "Cretsinger")

#Create a new variable for number of samples taken
CR1234 <- CR1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset CR1234 to include only Cretsinger samples collected with blue vane traps
CR1234BV <- CR1234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 20. (We have an extra sample because of 8/5/2016.)
length(unique(CR1234BV$Date))

#Create a table showing the number of unique species collected during each sample
CR1234BVcount <-  CR1234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR1234BVtable <- CR1234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR1234BVtablewide <- spread(CR1234BVtable, Binomial, n)

#Change CR1234BVtablewide to dataframe
CR1234BVtablewide <- as.data.frame(CR1234BVtablewide)

#Change row names to assigned sample number
CR1234BVtablewide <- CR1234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR1234BVtablewide[is.na(CR1234BVtablewide)] <- 0

#Convert to numeric
CR1234BVtablewide[] <- lapply(CR1234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(CR1234BVtablewide)

#-------------------------------------------------------------------#
#                              CRETSINGER                           #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset CR1234 to include only Cretsinger samples collected with bee bowls
CR1234BB <- CR1234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 20. (We have an extra sample because of 8/5/2016.)
length(unique(CR1234BB$Date))

#Create a table showing the number of species collected during each sample
CR1234BBcount <-  CR1234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR1234BBtable <- CR1234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR1234BBtablewide <- spread(CR1234BBtable, Binomial, n)

#Change CR1234BBtablewide to dataframe
CR1234BBtablewide <- as.data.frame(CR1234BBtablewide)

#Change row names to assigned sample number
CR1234BBtablewide <- CR1234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR1234BBtablewide[is.na(CR1234BBtablewide)] <- 0

#Convert to numeric
CR1234BBtablewide[] <- lapply(CR1234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(CR1234BBtablewide)

#-------------------------------------------------------------------#
#                              CRETSINGER                           #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset CR1234 to include only Cretsinger samples collected via non-targeted sweeping
CR1234NT <- CR1234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 20. (We have an extra sample because of 8/5/2016.)
length(unique(CR1234NT$Date))

#Create a table showing the number of species collected during each sample
CR1234NTcount <-  CR1234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR1234NTtable <- CR1234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR1234NTtablewide <- spread(CR1234NTtable, Binomial, n)

#Change CR1234NTtablewide to dataframe
CR1234NTtablewide <- as.data.frame(CR1234NTtablewide)

#Change row names to assigned sample number
CR1234NTtablewide <- CR1234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR1234NTtablewide[is.na(CR1234NTtablewide)] <- 0

#Convert to numeric
CR1234NTtablewide[] <- lapply(CR1234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(CR1234NTtablewide)

#-------------------------------------------------------------------#
#                              CRETSINGER                           #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset CR1234 to include only Cretsinger samples collected by emergence traps
CR1234ET <- CR1234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 4.
length(unique(CR1234ET$Date))

#Create a table showing the number of species collected during each sample
CR1234ETcount <-  CR1234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR1234ETtable <- CR1234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR1234ETtablewide <- spread(CR1234ETtable, Binomial, n)

#Change CR1234ETtablewide to dataframe
CR1234ETtablewide <- as.data.frame(CR1234ETtablewide)

#Change row names to assigned sample number
CR1234ETtablewide <- CR1234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR1234ETtablewide[is.na(CR1234ETtablewide)] <- 0

#Convert to numeric
CR1234ETtablewide[] <- lapply(CR1234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(CR1234ETtablewide)

#-------------------------------------------------------------------#
#                               PECKUMN                             #
#                              Blue Vane                            #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Peckumn samples
PE234 <- BeeIDs1234 %>%
  filter(Site == "Peckumn")

#Create a new variable for number of samples taken
PE234 <- PE234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Subset PE234 to include only Peckumn samples collected with blue vane traps
PE234BV <- PE234 %>%
  filter(Trap == "Blue vane")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(PE234BV$Date))

#Create a table showing the number of unique species collected during each sample
PE234BVcount <-  PE234BV %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PE234BVtable <- PE234BV %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PE234BVtablewide <- spread(PE234BVtable, Binomial, n)

#Change PE234BVtablewide to dataframe
PE234BVtablewide <- as.data.frame(PE234BVtablewide)

#Change row names to assigned sample number
PE234BVtablewide <- PE234BVtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PE234BVtablewide[is.na(PE234BVtablewide)] <- 0

#Convert to numeric
PE234BVtablewide[] <- lapply(PE234BVtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PE234BVtablewide)

#-------------------------------------------------------------------#
#                               PECKUMN                             #
#                              Bee Bowls                            #
#-------------------------------------------------------------------#
#Subset PE234 to include only Peckumn samples collected with bee bowls
PE234BB <- PE234 %>%
  filter(Trap == "Bowls")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(PE234BB$Date))

#Create a table showing the number of species collected during each sample
PE234BBcount <-  PE234BB %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PE234BBtable <- PE234BB %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PE234BBtablewide <- spread(PE234BBtable, Binomial, n)

#Change PE234BBtablewide to dataframe
PE234BBtablewide <- as.data.frame(PE234BBtablewide)

#Change row names to assigned sample number
PE234BBtablewide <- PE234BBtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PE234BBtablewide[is.na(PE234BBtablewide)] <- 0

#Convert to numeric
PE234BBtablewide[] <- lapply(PE234BBtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PE234BBtablewide)

#-------------------------------------------------------------------#
#                               PECKUMN                             #
#                        Non-Targeted Sweeping                      #
#-------------------------------------------------------------------#
#Subset PE234 to include only Peckumn samples collected via non-targeted sweeping
PE234NT <- PE234 %>%
  filter(Trap == "NT")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(PE234NT$Date))

#Create a table showing the number of species collected during each sample
PE234NTcount <-  PE234NT %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PE234NTtable <- PE234NT %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PE234NTtablewide <- spread(PE234NTtable, Binomial, n)

#Change PE234NTtablewide to dataframe
PE234NTtablewide <- as.data.frame(PE234NTtablewide)

#Change row names to assigned sample number
PE234NTtablewide <- PE234NTtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PE234NTtablewide[is.na(PE234NTtablewide)] <- 0

#Convert to numeric
PE234NTtablewide[] <- lapply(PE234NTtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PE234NTtablewide)

#-------------------------------------------------------------------#
#                               PECKUMN                             #
#                              Emergence                            #
#-------------------------------------------------------------------#
#Subset PE234 to include only Peckumn samples collected by emergence traps
PE234ET <- PE234 %>%
  filter(Trap == "Emergence")

#Determine number of unique dates to be sure it matches the original data sheet. Should be 4.
length(unique(PE234ET$Date))

#Create a table showing the number of species collected during each sample
PE234ETcount <-  PE234ET %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PE234ETtable <- PE234ET %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PE234ETtablewide <- spread(PE234ETtable, Binomial, n)

#Change PE234ETtablewide to dataframe
PE234ETtablewide <- as.data.frame(PE234ETtablewide)

#Change row names to assigned sample number
PE234ETtablewide <- PE234ETtablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PE234ETtablewide[is.na(PE234ETtablewide)] <- 0

#Convert to numeric
PE234ETtablewide[] <- lapply(PE234ETtablewide, function(x) as.numeric(as.character(x)))

#Create rarefaction curve
rarecurve(PE234ETtablewide)
