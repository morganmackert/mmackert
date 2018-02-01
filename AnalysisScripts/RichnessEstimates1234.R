#-------------------------------------------------------------------#
#                        Richness Estimates                         #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Research Question: How do richness estimates of the bee community vary between sites of differing vegetation diversities?

#Objectives:
#Determine Chao1 richness estimate and Inverse Simpson's Diversity Index for the bee community at each site
#Create models and graphs to visually represent the changing richness estimates

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(vegan)

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
Quadrats$Date <- mdy(Quadrats$Date)

#Add new column with only the year
BeeIDs$Year <- year(BeeIDs$Date)
Quadrats$Year <- year(Quadrats$Date)

#Change trap names for consistency
BeeIDs$Trap[BeeIDs$Trap == "Non-Target"] <- "NT"
BeeIDs$Trap[BeeIDs$Trap == "Emergence Trap"] <- "Emergence"
BeeIDs$Trap[BeeIDs$Trap == "Blue Vane"] <- "Blue vane"

#Change column names so they're not so goofy.
names(Quadrats)[names(Quadrats) == "X..Cover"] <- "Cover"
names(Quadrats)[names(Quadrats) == "X..Bare.Ground"] <- "Bare.Ground"
names(Quadrats)[names(Quadrats) == "Species.in.Strip...Not.in.Quadrats"] <- "Strip.Plants"

#Subset only years 1-4; BeeIDs without target bees, wasps, or unidentifiable specimens
BeeIDs1234 <- BeeIDs %>%
  filter(Year <= 2017) %>%
  filter(Trap != "Target") %>%
  filter(Trap != "Pitfall") %>%
  filter(Binomial != "Wasp") %>%
  filter(Family != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(Date != "2014-07-09") %>%
  filter(Date != "2014-08-12") %>%
  filter(Date != "2015-06-13") %>%
  filter(Date != "2015-06-10") %>%
  filter(Date != "2015-08-11")

Quadrats1234 <- Quadrats %>%
  filter(Year <= 2017)

#Create a table showing number of individuals of each species collected at each site
BeeIDs1234sppbysitedate <- BeeIDs1234 %>%
  group_by(Date, Site) %>%
  count(Binomial)

BeeIDs1234sppbysite <- BeeIDs1234sppbysitedate %>%
  group_by(Site, Binomial) %>%
  summarise(n = sum(n))

#Reformat from long to wide
BeeIDs1234sppbysitedatewide <- spread(BeeIDs1234sppbysitedate, Binomial, n)

#Convert to a dataframe
BeeIDs1234sppbysitedatewide <- as.data.frame(BeeIDs1234sppbysitedatewide)

#Fill NAs with 0
BeeIDs1234sppbysitedatewide[is.na(BeeIDs1234sppbysitedatewide)] <- 0

#Use tidyr to combine the Date and Site columns
BeeIDs1234sppbysitedatewide <- unite(BeeIDs1234sppbysitedatewide, Date.Site, c(Date, Site), remove = TRUE)

#Change row names to site name
BeeIDs1234sppbysitedatewide <- BeeIDs1234sppbysitedatewide %>%
  remove_rownames %>%
  column_to_rownames("Date.Site")

#Determine number of unique blooming species found in quadrats at each site, not including NAs
bsquadrats1234 <- Quadrats1234 %>%
  group_by(Site, Date) %>%
  summarise(TotalBS = length(unique(Species)))

#Estimate Inverse Simpson's Diversity Index
InvSimp1234 <- diversity(BeeIDs1234sppbysitedatewide, "inv")

#Move rownames to "Site" variable
InvSimp1234 <- as.data.frame(InvSimp1234)
InvSimp1234 <- rownames_to_column(InvSimp1234, var = "Date.Site")

#Separate Date.Site variable
InvSimp1234 <- InvSimp1234 %>%
  separate(Date.Site, c("Date", "Site"), sep = "_")

#Add number of blooming plant species to InvSimp data frame
InvSimpbsquadrats1234 <- merge(InvSimp1234, bsquadrats1234, by = c("Site", "Date"))

#Use lubridate to allow R to read dates
InvSimpbsquadrats1234$Date <- mdy(InvSimpbsquadrats1234$Date)

#Use lubridate to include "Year" variable
InvSimpbsquadrats1234$Year <- year(InvSimpbsquadrats1234$Date)

#Change Year to a factor
InvSimpbsquadrats1234$Year <- as.factor(InvSimpbsquadrats1234$Year)

#Graph with total number of blooming species at each site
InvSimpplotwithtotalveg <-  ggplot(InvSimpbsquadrats1234,
                                   aes (x = TotalBS,
                                        y = InvSimp1234)) +
  geom_point(aes(color = Year,
                 shape = Year),
             size = 3) +
  geom_smooth(method = "glm",
              se = FALSE,
              color = "black",
              size = 0.5) +
  scale_color_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c("darkorchid1", "darkgreen", "#000000", "#FFB90F")) +
  scale_shape_manual(labels = c("2014", "2015", "2016", "2017"),
                     values = c(15, 16, 17, 18)) +
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
