#-------------------------------------------------------------------#
#                     Richness Estimatess by Site                   #
#                             Years 1-3                             #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(tibble)
library(vegan)
library(multcompView)
library(ggplot2)

#Read in data
BeeIDs <- read.csv("Bees/Bee IDs.csv")
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet
Fulldata <- read.csv("Combined full data set.csv")
#Date = Date of sample
#Site = Site name
#Sampling.Period; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#X..Floral.Cover..in.10m2. = Average coverage of blooming forb/weed species in ten quadrats
#X..Blooming.species.in.quadrats = Number of forb/weed species in bloom within ten quadrats
#X..Bare.Ground..in.10m2. = Average bare ground coverage in ten quadrats
#Trapname.Abundance = Number of individual bees collected by specified trap/site/date
#Total.Abundance = Number of individual bees collected by all trap types at the specified site/date
#Trapname.Species.Richness = Number of bee species collected by specified trap/site/date
#Total.Species.Richness = Number of bee species collected by all trap types at the specified site/date
#Species.Name = Number of individuals of specified species collected at the specified site/date
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"))
#Date = Date of sample
#Year = Year of the study; 1 = 2014, 2 = 2015, 3 = 2016, 4 = 2017
#Sample; 1 = Early May, 2 = Late May, 3 = June, 4 = July, 5 = August
#Site = Site name
#Quadrat = Quadrat number; 1-10
#Species = Name of plant(s) in quadrat
#X..Cover = Percent coverage of each species within quadrat
#X..Bare.Ground = Percent coverage of bare ground within quadrat
#Species.in.Strip...Not.in.Quadrats = Blooming plant species occurring within the study strip, but not detected within the quadrats
#Outside.Species = Blooming plant species occurring elsewhere on the property

#Use lubridate to allow R to recognize the dates
BeeIDs$Date <- mdy(BeeIDs$Date)
Fulldata$Date <- mdy(Fulldata$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)

#Because we're sorting by "Site," we need to make sure naming conventions are consistent
BeeIDs %>%
  group_by(Site) %>%
  summarise()

#Same with "Trap"
BeeIDs %>%
  group_by(Trap) %>%
  summarise()

#We find that site names are good to go, but trap names need some work!
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"

#Change column names so they're not so goofy.
names(Fulldata)[names(Fulldata) == "X..Floral.Cover..in.10m2."] <- "Floral.Cover"
names(Fulldata)[names(Fulldata) == "X..Blooming.species.in.quadrats"] <- "Blooming.Species"
names(Fulldata)[names(Fulldata) == "X..Bare.Ground..in.10m2."] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset only years 1-3; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs123 <- BeeIDs %>%
  filter(Year <= 2016) %>%
  filter(Trap != "Target") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable")
years123 <- Fulldata %>%
  filter(Year <= 3)
Quadrats123 <- Quadrats %>%
  filter(Year <= 3)

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

#Determine number of unique dates to be sure it matches the original data sheet. Should be 16. (Includes pitfall collection dates)
length(unique(PL123$Date))

#Create a table showing the number of unique species collected during each sample
PL123count <-  PL123 %>%
  group_by(Date) %>%
  summarise(Number_Species = n_distinct(Binomial))

#Create a table listing each species and number of individuals collected during each sample
PL123table <- PL123 %>%
  group_by(Date) %>%
  count(Binomial)

#Reformat from long to wide
PL123tablewide <- spread(PL123table, Binomial, n)

#Change PL1234BVtablewide to dataframe
PL123tablewide <- as.data.frame(PL123tablewide)

#Change row names to assigned sample number
PL123tablewide <- PL123tablewide %>%
  remove_rownames %>%
  column_to_rownames("Date")

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
  group_by(Site) %>%
  mutate(Sampling_Day = as.factor(dense_rank(Date)))

#Determine number of unique dates to be sure it matches the original data sheet. Should be 16.
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

#-------------------------------------------------------------------#
#                              All Sites                            #
#-------------------------------------------------------------------#

#Create a table showing number of individuals of each species species collected at each site
BeeIDs123indspecSD <- BeeIDs123 %>%
  group_by(Date, Site) %>%
  count(Binomial)
BeeIDs123spec <- BeeIDs123indspecSD %>%
  group_by(Site, Binomial) %>%
  summarise(n = sum(n))

#Reformat from long to wide
BeeIDS123specwide <- spread(BeeIDs123spec, Binomial, n)

#Convert to a dataframe
BeeIDS123specwide <- as.data.frame(BeeIDS123specwide)

#Fill NAs with 0
BeeIDS123specwide[is.na(BeeIDS123specwide)] <- 0

#Change row names to site name
BeeIDS123specwide <- BeeIDS123specwide %>%
  remove_rownames %>%
  column_to_rownames("Site")

#Estimate Chao1 and ACE richness
Chao1 <- estimateR(BeeIDS123specwide)

#Transpose axes in Chao1
tChao1 <- t(Chao1)

#Move rownames into "Site" variable
tChao1 <- as.data.frame(tChao1)
tChao1 <- rownames_to_column(tChao1, var = "Site")

#Graph it
#Sites are organized by increasing number of blooming plants present at each site.  1-10 is alphabetical.
tChao1plot <- ggplot(tChao1,
                     aes (x = Site,
                          y = S.chao1)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  scale_x_discrete(limits = c("Bowman", "McClellan", "Sheller", "Elkader", "Sloan", "Kaldenberg", "Cretsinger", "Greving", "Plunkett", "Peckumn", "NealSmith")) +
  labs(x = "Site (increasing blooming plant diversity)",
       y = "Chao1 Richness Estimate") +
  ggtitle("Chao1 Richness Estimate at Each Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 45,
                                   hjust = 1))
tChao1plot

#Plot another way!
#Determine number of unique blooming species found in quadrats at each site, not including NAs
bsquadrats123 <- Quadrats123 %>%
  group_by(Site) %>%
  filter(!is.na(Species)) %>%
  summarise(TotalBS = length(unique(Species)))

#Add total average number of blooming plant species to tChao1 data frame
tChao1 <- full_join(tChao1, bsquadrats123, by = "Site")

#Model for relationship between Chao1 richness and average vegetation
tChao1plotwithavgvegmodel <- lm(S.chao1 ~ AverageVeg, data = tChao1)
summary(tChao1plotwithavgvegmodel)

#Model for relationship between Chao1 richness and total numaber of blooming species at each site
tChao1plotwithtotalvegmodel <- lm(S.chao1 ~ TotalBS, data = tChao1)
summary(tChao1plotwithtotalvegmodel)

#Graph with average number of blooming species at eacah site
tChao1plotwithavgveg <- ggplot(tChao1,
                     aes (x = AverageVeg,
                          y = S.chao1)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Average Number of Blooming Plant Species",
       y = "Chao1 Richness Estimate") +
  ggtitle("Chao1 Richness Estimate with \nIncreasing Blooming Plant Diversity") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   hjust = 1))
tChao1plotwithavgveg

#Graph with total number of blooming species at each site
tChao1plotwithtotalveg <- ggplot(tChao1,
                                 aes(x = TotalBS,
                                     y = S.chao1)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Blooming Plant Species",
       y = "Chao1 Richness Estimate") +
  ggtitle("Chao1 Richness Estimate with \nIncreasing Blooming Plant Diversity") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   hjust = 1))
tChao1plotwithtotalveg

#Determine significance levels
###FIGURE THIS OUT MORGAN
AOV <- aov(S.obs ~ Site, data = tChao1)
tChao1Letters <- data.frame("Letters" = multcompLetters(extract_p(TukeyHSD(AOV)$"Site"))$"Letters")

AOV <- aov(n ~ Site, data = BeeIDs123spec)
summary(AOV)

#Estimate Inverse Simpson's Diversity Index
InvSimp <- diversity(BeeIDS123specwide, "inv")

#Move rownames to "Site" variable
InvSimp <- as.data.frame(InvSimp)
InvSimp <- rownames_to_column(InvSimp, var = "Site")

#Graph it!
InvSimpplot <- ggplot(InvSimp,
                      aes (x = Site,
                           y = InvSimp)) +
  geom_point(size = 3) +
  theme_bw() +
  scale_x_discrete(limits = c("Bowman", "McClellan", "Sheller", "Elkader", "Sloan", "Kaldenberg", "Cretsinger", "Greving", "Plunkett", "Peckumn", "NealSmith")) +
  labs(x = "Site (increasing blooming plant diversity)",
       y = "Inverse Simpson's Diversity Index") +
  ggtitle("Inverse Simpson's Diversity Index at Each Site") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   angle = 45,
                                   hjust = 1))
InvSimpplot

#Add average number of blooming plant species to InvSimp data frame
InvSimp <- full_join(InvSimp, bsquadrats123, by = "Site")

#Model for relationship between InvSimp and average vegetation
Invsimpwithavgvegmodel <- lm(InvSimp ~ AverageVeg, data = InvSimp)
summary(Invsimpwithavgvegmodel)

#Model for relationhip between InvSimp and total number of blooming plant species at each site
InvSimpwithtotalvegmodel <- lm(InvSimp ~ TotalBS, data = InvSimp)
summary(InvSimpwithtotalvegmodel)

#Graph with average number of blooming species at each site
InvSimpplotwithavgveg <-  ggplot(InvSimp,
                              aes (x = AverageVeg,
                                   y = InvSimp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Average Number of Blooming Plant Species",
       y = "Inverse Simpson's Diversity Index") +
  ggtitle("Inverse Simpson's Diversity Index with \nIncreasing Blooming Plant Diversity") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   hjust = 1)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align = 0.5)
InvSimpplotwithavgveg

#Graph with total number of blooming species at each site
InvSimpplotwithtotalveg <-  ggplot(InvSimp,
                                 aes (x = TotalBS,
                                      y = InvSimp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  theme_bw() +
  labs(x = "Number of Blooming Plant Species",
       y = "Inverse Simpson's Diversity Index") +
  ggtitle("Inverse Simpson's Diversity Index with \nIncreasing Blooming Plant Diversity") +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(size = 11,
                                   hjust = 1)) +
  theme(legend.text = element_text(size = 10)) +
  theme(legend.title = element_text(face = "bold")) +
  theme(legend.title.align = 0.5)
InvSimpplotwithtotalveg