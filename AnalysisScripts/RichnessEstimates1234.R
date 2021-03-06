#-------------------------------------------------------------------#
#                        Richness Estimates                         #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Research Question: How do richness estimates of the bee community vary between sites of differing vegetation diversities?

#Objectives:
#Determine Chao1 richness estimate and Inverse Simpson's Diversity Index for the bee community at each site
#Create models and graphs to visually represent the changing richness estimates

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)
library(vegan)
library(ggplot2)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = T, na.strings = c("", "NA"))
Quadrats <- read.csv("Plants/Quadrats.csv", header = T, na.strings = c("", "NA"))

#Use lubridate to allow R to recognize the dates
Bees$Date <- mdy(Bees$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Add new column with only the year
Bees$Year <- year(Bees$Date)
Quadrats$Year <- year(Quadrats$Date)

#Subset only years 1-4; BeeIDs without target bees, wasps, or unidentifiable specimens
bees <- Bees %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Binomial))

#Create a table showing number of individuals of each species collected at each site
bees.sitedate <- bees %>%
  group_by(Date, Site) %>%
  count(Binomial)

#bees.site <- bees.sitedate %>%
  #group_by(Site, Binomial) %>%
  #summarise(n = sum(n))

#Reformat from long to wide
bees.sitedate.wide <- spread(bees.sitedate, Binomial, n)

#Convert to a dataframe
bees.sitedate.wide <- as.data.frame(bees.sitedate.wide)

#Fill NAs with 0
bees.sitedate.wide[is.na(bees.sitedate.wide)] <- 0

#Use tidyr to combine the Date and Site columns
bees.sitedate.wide <- unite(bees.sitedate.wide, Date.Site, c(Date, Site), remove = TRUE)

#Change row names to site name
bees.sitedate.wide <- bees.sitedate.wide %>%
  column_to_rownames("Date.Site")

#Determine number of unique blooming species found in quadrats at each site, not including NAs
blooms <- Quadrats %>%
  group_by(Site, Date) %>%
  summarise(number.blooms = length(unique(Species)))

#Estimate Inverse Simpson's Diversity Index
inv.simp <- diversity(bees.sitedate.wide, "inv")

#Move rownames to "Site" variable
inv.simp <- as.data.frame(inv.simp)
inv.simp <- rownames_to_column(inv.simp, var = "Date.Site")

#Separate Date.Site variable
inv.simp <- inv.simp %>%
  separate(Date.Site, c("Date", "Site"), sep = "_")

#Convert inv.simp date to Date format
inv.simp$Date <- ymd(inv.simp$Date)

#Add number of blooming plant species to InvSimp data frame
inv.simp.blooms <- full_join(inv.simp, blooms, by = c("Site", "Date"))

#Use lubridate to include "Year" variable
inv.simp.blooms$Year <- year(inv.simp.blooms$Date)

#Change Year to a factor
inv.simp.blooms$Year <- as.factor(inv.simp.blooms$Year)

#Insert 0 for NAs
inv.simp.blooms[is.na(inv.simp.blooms)] <- 0

#Graph with total number of blooming species at each site
inv.simp.blooms.plot <-  ggplot(inv.simp.blooms,
                                   aes (x = blooms,
                                        y = inv.simp)) +
  geom_point(size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  #scale_color_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     #values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F", "cornflowerblue")) +
  #scale_shape_manual(labels = c("2014", "2015", "2016", "2017", "2018"),
                     #values = c(15, 1, 17, 18, 25)) +
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
inv.simp.blooms.plot

#Model for Inverse Simplson's Diversity Index predicted by number of blooming Species
BSonInvSimp1234model <- lmer(InvSimp1234 ~ TotalBS + (1|Site) + (1|Year),
                             data = InvSimpbsquadrats1234)
summary(BSonInvSimp1234model)

#Null model not including number of blooming species
BSonInvSimp1234null <- lmer(InvSimp1234 ~ (1|Site) + (1|Year),
                       data = InvSimpbsquadrats1234)
summary(BSonInvSimp1234null)

#Likelihood ratio test between the full and null models
anova(BSonInvSimp1234null, BSonInvSimp1234model)

#Plot residuals from the full model to ensure no deviations from normality
plot(fitted(BSonInvSimp1234model, residuals(BSonInvSimp1234model)))

#Use MuMIn to get R-squared value of full model
r.squaredGLMM(BSonInvSimp1234model)

#Data dictionary ####
#BeeIDs.csv:
#Number = Individual identification number assigned to each specimen
#Date = Date of sample
#Site = Site name
#Trap = Trap type in which each specimen was collected
#Sex = Sex of the specimen; M = male, F = female
#Family = Taxonomic family to which each specimen belongs
#Genus = Taxonimic genus to which each specimen belongs
#Species = Taxonomic species to which each specimen belongs
#Binomial = Combined genus and species to create specific epithet

#Quadrats.csv
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
#Old code ####
#Change trap names for consistency
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"


#Change column names so they're not so goofy.
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"