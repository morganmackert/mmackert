#-------------------------------------------------------------------#
#                     Richness Estimatess by Site                   #
#                             Years 1-3                             #
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

#Add new column with only the year
BeeIDs1234$Year <- year(BeeIDs1234$Date)

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

#Select only years 2014-2016
BeeIDs123 <- BeeIDs1234 %>%
  filter(Year <= "2016")

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Plunkett samples
PL123 <- BeeIDs123 %>%
  filter(Site == "Plunkett")

#Create a new variable for number of samples taken
PL123 <- PL123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 21.
length(unique(PL123$Date))

#Create a table showing the number of unique species collected during each sample
PL123count <-  PL123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL123table <- PL123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
PL123tablewide <- spread(PL123table, Binomial, n)

#Change PL1234BVtablewide to dataframe
PL123tablewide <- as.data.frame(PL123tablewide)

#Change row names to assigned sample number
PL123tablewide <- PL123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
PL123tablewide[is.na(PL123tablewide)] <- 0

#Convert to numeric
PL123tablewide[] <- lapply(PL123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
PL123ChACE <- estimateR(PL123tablewide)

#Jackknife estimate
PL123jack <- specpool(PL123tablewide)

#Diversity indices
PL123simp <- diversity(PL123tablewide, "simpson")
PL123inv <- diversity(PL123tablewide, "inv")
PL123shan <- diversity(PL123tablewide, "shannon")

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Bowman samples
BO123 <- BeeIDs123 %>%
  filter(Site == "Bowman")

#Create a new variable for number of samples taken
BO123 <- BO123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(BO123$Date))

#Create a table showing the number of unique species collected during each sample
BO123count <-  BO123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
BO123table <- BO123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
BO123tablewide <- spread(BO123table, Binomial, n)

#Change BO123tablewide to dataframe
BO123tablewide <- as.data.frame(BO123tablewide)

#Change row names to assigned sample number
BO123tablewide <- BO123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
BO123tablewide[is.na(BO123tablewide)] <- 0

#Convert to numeric
BO123tablewide[] <- lapply(BO123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
BO123ChACE <- estimateR(BO123tablewide)

#Jackknife estimate
BO123jack <- specpool(BO123tablewide)

#Diversity indices
BO123simp <- diversity(BO123tablewide, "simpson")
BO123inv <- diversity(BO123tablewide, "inv")
BO123shan <- diversity(BO123tablewide, "shannon")

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Subset BeeIDs1234 to include only Kaldenberg samples
KA123 <- BeeIDs123 %>%
  filter(Site == "Kaldenberg")

#Create a new variable for number of samples taken
KA123 <- KA123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(KA123$Date))

#Create a table showing the number of unique species collected during each sample
KA123count <-  KA123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
KA123table <- KA123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
KA123tablewide <- spread(KA123table, Binomial, n)

#Change KA123tablewide to dataframe
KA123tablewide <- as.data.frame(KA123tablewide)

#Change row names to assigned sample number
KA123tablewide <- KA123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
KA123tablewide[is.na(KA123tablewide)] <- 0

#Convert to numeric
KA123tablewide[] <- lapply(KA123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
KA123ChACE <- estimateR(KA123tablewide)

#Jackknife estimate
KA123jack <- specpool(KA123tablewide)

#Diversity indices
KA123simp <- diversity(KA123tablewide, "simpson")
KA123inv <- diversity(KA123tablewide, "inv")
KA123shan <- diversity(KA123tablewide, "shannon")

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only McClellan samples
MC123 <- BeeIDs123 %>%
  filter(Site == "McClellan")

#Create a new variable for number of samples taken
MC123 <- MC123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 18.
length(unique(MC123$Date))

#Create a table showing the number of unique species collected during each sample
MC123count <-  MC123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
MC123table <- MC123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
MC123tablewide <- spread(MC123table, Binomial, n)

#Change MC123tablewide to dataframe
MC123tablewide <- as.data.frame(MC123tablewide)

#Change row names to assigned sample number
MC123tablewide <- MC123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
MC123tablewide[is.na(MC123tablewide)] <- 0

#Convert to numeric
MC123tablewide[] <- lapply(MC123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
MC123ChACE <- estimateR(MC123tablewide)

#Jackknife estimate
MC123jack <- specpool(MC123tablewide)

#Diversity indices
MC123simp <- diversity(MC123tablewide, "simpson")
MC123inv <- diversity(MC123tablewide, "inv")
MC123shan <- diversity(MC123tablewide, "shannon")

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Sloan samples
SL123 <- BeeIDs123 %>%
  filter(Site == "Sloan")

#Create a new variable for number of samples taken
SL123 <- SL123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SL123$Date))

#Create a table showing the number of unique species collected during each sample
SL123count <-  SL123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SL123table <- SL123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SL123tablewide <- spread(SL123table, Binomial, n)

#Change SL123tablewide to dataframe
SL123tablewide <- as.data.frame(SL123tablewide)

#Change row names to assigned sample number
SL123tablewide <- SL123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SL123tablewide[is.na(SL123tablewide)] <- 0

#Convert to numeric
SL123tablewide[] <- lapply(SL123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
SL123ChACE <- estimateR(SL123tablewide)

#Jackknife estimate
SL123jack <- specpool(SL123tablewide)

#Diversity indices
SL123simp <- diversity(SL123tablewide, "simpson")
SL123inv <- diversity(SL123tablewide, "inv")
SL123shan <- diversity(SL123tablewide, "shannon")


#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Sheller samples
SH123 <- BeeIDs123 %>%
  filter(Site == "Sheller")

#Create a new variable for number of samples taken
SH123 <- SH123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 19.
length(unique(SH123$Date))

#Create a table showing the number of unique species collected during each sample
SH123count <-  SH123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
SH123table <- SH123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
SH123tablewide <- spread(SH123table, Binomial, n)

#Change SH123tablewide to dataframe
SH123tablewide <- as.data.frame(SH123tablewide)

#Change row names to assigned sample number
SH123tablewide <- SH123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
SH123tablewide[is.na(SH123tablewide)] <- 0

#Convert to numeric
SH123tablewide[] <- lapply(SH123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
SH123ChACE <- estimateR(SH123tablewide)

#Jackknife estimate
SH123jack <- specpool(SH123tablewide)

#Diversity indices
SH123simp <- diversity(SH123tablewide, "simpson")
SH123inv <- diversity(SH123tablewide, "inv")
SH123shan <- diversity(SH123tablewide, "shannon")

#-------------------------------------------------------------------#
#                            Cretsinger                             #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Cretsinger samples
CR123 <- BeeIDs123 %>%
  filter(Site == "Cretsinger")

#Create a new variable for number of samples taken
CR123 <- CR123 %>%
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 20. (We have an extra sample because of 8/5/2016.)
length(unique(CR123$Date))

#Create a table showing the number of unique species collected during each sample
CR123count <-  CR123 %>%
  group_by(Sampling_Day) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
CR123table <- CR123 %>%
  group_by(Sampling_Day) %>%
  count(Binomial)

#Reformat from long to wide
CR123tablewide <- spread(CR123table, Binomial, n)

#Change CR123tablewide to dataframe
CR123tablewide <- as.data.frame(CR123tablewide)

#Change row names to assigned sample number
CR123tablewide <- CR123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Sampling_Day")

#Fill NAs with 0
CR123tablewide[is.na(CR123tablewide)] <- 0

#Convert to numeric
CR123tablewide[] <- lapply(CR123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
CR123ChACE <- estimateR(CR123tablewide)

#Jackknife estimate
CR123jack <- specpool(CR123tablewide)

#Diversity indices
CR123simp <- diversity(CR123tablewide, "simpson")
CR123inv <- diversity(CR123tablewide, "inv")
CR123shan <- diversity(CR123tablewide, "shannon")

#-------------------------------------------------------------------#
#                        Greving and Peckumn                        #
#-------------------------------------------------------------------#
#Subset BeeIDs123 to include only Greving and Peckumn samples
GRPE123 <- BeeIDs123 %>%
  filter(Site %in% c("Greving", "Peckumn"))

#Create a new variable for number of samples taken
GRPE123 <- GRPE123 %>%
  #group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 15.
length(unique(GRPE123$Date))

#Create a table showing the number of unique species collected during each sample
GRPE123count <-  GRPE123 %>%
  group_by(Date) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
GRPE123table <- GRPE123 %>%
  group_by(Date) %>%
  count(Binomial)

#Reformat from long to wide
GRPE123tablewide <- spread(GRPE123table, Binomial, n)

#Change GRPE123tablewide to dataframe
GRPE123tablewide <- as.data.frame(GRPE123tablewide)

#Remove Date column
GRPE123tablewide <- subset(GRPE123tablewide, select = -Date)

#Fill NAs with 0
GRPE123tablewide[is.na(GRPE123tablewide)] <- 0

#Convert to numeric
GRPE123tablewide[] <- lapply(GRPE123tablewide, function(x) as.numeric(as.character(x)))

#Estimate Chao1 and ACE richness
GRPE123ChACE <- estimateR(GRPE123tablewide)

#Jackknife estimate
GRPE123jack <- specpool(GRPE123tablewide)

#Diversity indices
GRPE123simp <- diversity(GRPE123tablewide, "simpson")
GRPE123inv <- diversity(GRPE123tablewide, "inv")
GRPE123shan <- diversity(GRPE123tablewide, "shannon")
