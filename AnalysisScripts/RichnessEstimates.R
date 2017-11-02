#-------------------------------------------------------------------#
#                     Richness Estimatess by Site                   #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

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

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Plunkett samples
PL1234 <- BeeIDs1234 %>%
  filter(Site == "Plunkett")

#Create a new variable for number of samples taken
PL1234 <- PL1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 21.
length(unique(PL1234$Date))

#Create a table showing the number of unique species collected during each sample
PL1234count <-  PL1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL1234table <- PL1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL1234tablewide <- spread(PL1234table, Binomial, n)

#Change PL1234BVtablewide to dataframe
PL1234tablewide <- as.data.frame(PL1234tablewide)

#Change row names to assigned sample number
PL1234tablewide <- PL1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL1234tablewide[is.na(PL1234tablewide)] <- 0

#Convert to numeric
PL1234tablewide[] <- lapply(PL1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
PL1234CHACE <- estimateR(PL1234tablewide)

#Jackknife estimate
PL1234jack <- specpool(PL1234tablewide)

#Diversity indices
PL1234simp <- diversity(PL1234tablewide, "simpson")
PL1234inv <- diversity(PL1234tablewide, "inv")
PL1234shan <- diversity(PL1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Bowman samples
BO1234 <- BeeIDs1234 %>%
  filter(Site == "Bowman")

#Create a new variable for number of samples taken
BO1234 <- BO1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(BO1234$Date))

#Create a table showing the number of unique species collected during each sample
BO1234count <-  BO1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO1234table <- BO1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO1234tablewide <- spread(BO1234table, Binomial, n)

#Change BO1234tablewide to dataframe
BO1234tablewide <- as.data.frame(BO1234tablewide)

#Change row names to assigned sample number
BO1234tablewide <- BO1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO1234tablewide[is.na(BO1234tablewide)] <- 0

#Convert to numeric
BO1234tablewide[] <- lapply(BO1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
BO1234CHACE <- estimateR(BO1234tablewide)

#Jackknife estimate
specpool(BO1234tablewide)

#Diversity indices
BO1234simp <- diversity(BO1234tablewide, "simpson")
BO1234inv <- diversity(BO1234tablewide, "inv")
BO1234shan <- diversity(BO1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Kaldenberg samples
KA1234 <- BeeIDs1234 %>%
  filter(Site == "Kaldenberg")

#Create a new variable for number of samples taken
KA1234 <- KA1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(KA1234$Date))

#Create a table showing the number of unique species collected during each sample
KA1234count <-  KA1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA1234table <- KA1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA1234tablewide <- spread(KA1234table, Binomial, n)

#Change KA1234tablewide to dataframe
KA1234tablewide <- as.data.frame(KA1234tablewide)

#Change row names to assigned sample number
KA1234tablewide <- KA1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA1234tablewide[is.na(KA1234tablewide)] <- 0

#Convert to numeric
KA1234tablewide[] <- lapply(KA1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(KA1234tablewide)

#Jackknife estimate
specpool(KA1234tablewide)

#Diversity indices
KA1234simp <- diversity(KA1234tablewide, "simpson")
KA1234inv <- diversity(KA1234tablewide, "inv")
KA1234shan <- diversity(KA1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only McClellan samples
MC1234 <- BeeIDs1234 %>%
  filter(Site == "McClellan")

#Create a new variable for number of samples taken
MC1234 <- MC1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(MC1234$Date))

#Create a table showing the number of unique species collected during each sample
MC1234count <-  MC1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC1234table <- MC1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC1234tablewide <- spread(MC1234table, Binomial, n)

#Change MC1234tablewide to dataframe
MC1234tablewide <- as.data.frame(MC1234tablewide)

#Change row names to assigned sample number
MC1234tablewide <- MC1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC1234tablewide[is.na(MC1234tablewide)] <- 0

#Convert to numeric
MC1234tablewide[] <- lapply(MC1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(MC1234tablewide)

#Jackknife estimate
specpool(MC1234tablewide)

#Diversity indices
MC1234simp <- diversity(MC1234tablewide, "simpson")
MC1234inv <- diversity(MC1234tablewide, "inv")
MC1234shan <- diversity(MC1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Sloan samples
SL1234 <- BeeIDs1234 %>%
  filter(Site == "Sloan")

#Create a new variable for number of samples taken
SL1234 <- SL1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SL1234$Date))

#Create a table showing the number of unique species collected during each sample
SL1234count <-  SL1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL1234table <- SL1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL1234tablewide <- spread(SL1234table, Binomial, n)

#Change SL1234tablewide to dataframe
SL1234tablewide <- as.data.frame(SL1234tablewide)

#Change row names to assigned sample number
SL1234tablewide <- SL1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL1234tablewide[is.na(SL1234tablewide)] <- 0

#Convert to numeric
SL1234tablewide[] <- lapply(SL1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(SL1234tablewide)

#Jackknife estimate
specpool(SL1234tablewide)

#Diversity indices
SL1234simp <- diversity(SL1234tablewide, "simpson")
SL1234inv <- diversity(SL1234tablewide, "inv")
SL1234shan <- diversity(SL1234tablewide, "shannon")


#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Sheller samples
SH1234 <- BeeIDs1234 %>%
  filter(Site == "Sheller")

#Create a new variable for number of samples taken
SH1234 <- SH1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SH1234$Date))

#Create a table showing the number of unique species collected during each sample
SH1234count <-  SH1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH1234table <- SH1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH1234tablewide <- spread(SH1234table, Binomial, n)

#Change SH1234tablewide to dataframe
SH1234tablewide <- as.data.frame(SH1234tablewide)

#Change row names to assigned sample number
SH1234tablewide <- SH1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH1234tablewide[is.na(SH1234tablewide)] <- 0

#Convert to numeric
SH1234tablewide[] <- lapply(SH1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(SH1234tablewide)

#Jackknife estimate
specpool(SH1234tablewide)

#Diversity indices
SH1234simp <- diversity(SH1234tablewide, "simpson")
SH1234inv <- diversity(SH1234tablewide, "inv")
SH1234shan <- diversity(SH1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                            Cretsinger                             #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Cretsinger samples
CR1234 <- BeeIDs1234 %>%
  filter(Site == "Cretsinger")

#Create a new variable for number of samples taken
CR1234 <- CR1234 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 20. (We have an extra sample because of 8/5/2016.)
length(unique(CR1234$Date))

#Create a table showing the number of unique species collected during each sample
CR1234count <-  CR1234 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR1234table <- CR1234 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR1234tablewide <- spread(CR1234table, Binomial, n)

#Change CR1234tablewide to dataframe
CR1234tablewide <- as.data.frame(CR1234tablewide)

#Change row names to assigned sample number
CR1234tablewide <- CR1234tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR1234tablewide[is.na(CR1234tablewide)] <- 0

#Convert to numeric
CR1234tablewide[] <- lapply(CR1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(CR1234tablewide)

#Jackknife estimate
specpool(CR1234tablewide)

#Diversity indices
CR1234simp <- diversity(CR1234tablewide, "simpson")
CR1234inv <- diversity(CR1234tablewide, "inv")
CR1234shan <- diversity(CR1234tablewide, "shannon")

#-------------------------------------------------------------------#
#                        Greving and Peckumn                        #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Greving and Peckumn samples
GRPE1234 <- BeeIDs1234 %>%
  filter(Site %in% c("Greving", "Peckumn"))

#Create a new variable for number of samples taken
GRPE1234 <- GRPE1234 %>%
  #group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(GRPE1234$Date))

#Create a table showing the number of unique species collected during each sample
GRPE1234count <-  GRPE1234 %>%
  group_by(Date) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
GRPE1234table <- GRPE1234 %>%
  group_by(Date) %>%
  count(Binomial)

#Reformat from long to wide
GRPE1234tablewide <- spread(GRPE1234table, Binomial, n)

#Change GRPE1234tablewide to dataframe
GRPE1234tablewide <- as.data.frame(GRPE1234tablewide)

#Remove Date column
GRPE1234tablewide <- subset(GRPE1234tablewide, select = -Date)

#Fill NAs with 0
GRPE1234tablewide[is.na(GRPE1234tablewide)] <- 0

#Convert to numeric
GRPE1234tablewide[] <- lapply(GRPE1234tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
estimateR(GRPE1234tablewide)

#Jackknife estimate
specpool(GRPE1234tablewide)

#Diversity indices
GRPE1234simp <- diversity(GRPE1234tablewide, "simpson")
GRPE1234inv <- diversity(GRPE1234tablewide, "inv")
GRPE1234shan <- diversity(GRPE1234tablewide, "shannon")