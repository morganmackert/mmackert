#####################################################################
#                            Bee Richness                           #
#                               Year 3                              #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data/Bees")

#Load libraries
library(data.table)

#Read in data
IDs2016 <- read.csv("2016/2016 Bee IDs.csv")

#Remove "Notes" column as well as additional empty columns
IDs2016 <- IDs2016[,-(10:29)]

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Plunkett
PlunkettIDs2016 <- IDs2016[IDs2016$Site == "Plunkett",]

#Determine number of families in "Family" column
PlunkettFR2016 <- aggregate(data.frame(count = PlunkettIDs2016$Family), list(value = PlunkettIDs2016$Family), length)

#Determine number of genera in "Genus" column
PlunkettGR2016 <- aggregate(data.frame(count = PlunkettIDs2016$Genus), list(value = PlunkettIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
PlunkettSR2016 <- aggregate(data.frame(count = PlunkettIDs2016$Binomial), list(value = PlunkettIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Bowman
BowmanIDs2016 <- IDs2016[IDs2016$Site == "Bowman",]

#Determine number of families in "Family" column
BowmanFR2016 <- aggregate(data.frame(count = BowmanIDs2016$Family), list(value = BowmanIDs2016$Family), length)

#Determine number of genera in "Genus" column
BowmanGR2016 <- aggregate(data.frame(count = BowmanIDs2016$Genus), list(value = BowmanIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
BowmanSR2016 <- aggregate(data.frame(count = BowmanIDs2016$Binomial), list(value = BowmanIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Kaldenberg
KaldenbergIDs2016 <- IDs2016[IDs2016$Site == "Kaldenberg",]

#Determine number of families in "Family" column
KaldenbergFR2016 <- aggregate(data.frame(count = KaldenbergIDs2016$Family), list(value = KaldenbergIDs2016$Family), length)

#Determine number of genera in "Genus" column
KaldenbergGR2016 <- aggregate(data.frame(count = KaldenbergIDs2016$Genus), list(value = KaldenbergIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
KaldenbergSR2016 <- aggregate(data.frame(count = KaldenbergIDs2016$Binomial), list(value = KaldenbergIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Kaldenberg
McClellanIDs2016 <- IDs2016[IDs2016$Site == "McClellan",]

#Determine number of families in "Family" column
McClellanFR2016 <- aggregate(data.frame(count = McClellanIDs2016$Family), list(value = McClellanIDs2016$Family), length)

#Determine number of genera in "Genus" column
McClellanGR2016 <- aggregate(data.frame(count = McClellanIDs2016$Genus), list(value = McClellanIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
McClellanSR2016 <- aggregate(data.frame(count = McClellanIDs2016$Binomial), list(value = McClellanIDs2016$Binomial), length)


#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Sloan
SloanIDs2016 <- IDs2016[IDs2016$Site == "Sloan",]

#Determine number of families in "Family" column
SloanFR2016 <- aggregate(data.frame(count = SloanIDs2016$Family), list(value = SloanIDs2016$Family), length)

#Determine number of genera in "Genus" column
SloanGR2016 <- aggregate(data.frame(count = SloanIDs2016$Genus), list(value = SloanIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
SloanSR2016 <- aggregate(data.frame(count = SloanIDs2016$Binomial), list(value = SloanIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Sheller
ShellerIDs2016 <- IDs2016[IDs2016$Site == "Sheller",]

#Determine number of families in "Family" column
ShellerFR2016 <- aggregate(data.frame(count = ShellerIDs2016$Family), list(value = ShellerIDs2016$Family), length)

#Determine number of genera in "Genus" column
ShellerGR2016 <- aggregate(data.frame(count = ShellerIDs2016$Genus), list(value = ShellerIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
ShellerSR2016 <- aggregate(data.frame(count = ShellerIDs2016$Binomial), list(value = ShellerIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Cretsinger
CretsingerIDs2016 <- IDs2016[IDs2016$Site == "Cretsinger",]

#Determine number of families in "Family" column
CretsingerFR2016 <- aggregate(data.frame(count = CretsingerIDs2016$Family), list(value = CretsingerIDs2016$Family), length)

#Determine number of genera in "Genus" column
CretsingerGR2016 <- aggregate(data.frame(count = CretsingerIDs2016$Genus), list(value = CretsingerIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
CretsingerSR2016 <- aggregate(data.frame(count = CretsingerIDs2016$Binomial), list(value = CretsingerIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Subset IDs2016 to show only bees collected from Peckumn
PeckumnIDs2016 <- IDs2016[IDs2016$Site == "Peckumn",]

#Determine number of families in "Family" column
PeckumnFR2016 <- aggregate(data.frame(count = PeckumnIDs2016$Family), list(value = PeckumnIDs2016$Family), length)

#Determine number of genera in "Genus" column
PeckumnGR2016 <- aggregate(data.frame(count = PeckumnIDs2016$Genus), list(value = PeckumnIDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
PeckumnSR2016 <- aggregate(data.frame(count = PeckumnIDs2016$Binomial), list(value = PeckumnIDs2016$Binomial), length)

#-------------------------------------------------------------------#
#                              All Sites                            #
#-------------------------------------------------------------------#
#Determine number of families in "Family" column
IDs2016FR <- aggregate(data.frame(count = IDs2016$Family), list(value = IDs2016$Family), length)

#Determine number of genera in "Genus" column
IDs2016GR <- aggregate(data.frame(count = IDs2016$Genus), list(value = IDs2016$Genus), length)

#Determine number of unique species in "Binomial" column
IDs2016SR <- aggregate(data.frame(count = IDs2016$Binomial), list(value = IDs2016$Binomial), length)

#####################################################################
#                            Bee Richness                           #
#                               Year 4                              #
#####################################################################

#Read in data
IDs2017 <- read.csv("2017/2017 Bee IDs.csv")

#Remove "Notes" column as well as additional empty columns
IDs2017 <- IDs2017[,-(10:26)]

#Remove empty rows at the end
IDs2017 <- IDs2017[-(4008:4300),]

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Plunkett
PlunkettIDs2017 <- IDs2017[IDs2017$Site == "Plunkett",]

#Determine number of families in "Family" column
PlunkettFR2017 <- aggregate(data.frame(count = PlunkettIDs2017$Family), list(value = PlunkettIDs2017$Family), length)

#Determine number of genera in "Genus" column
PlunkettGR2017 <- aggregate(data.frame(count = PlunkettIDs2017$Genus), list(value = PlunkettIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
PlunkettSR2017 <- aggregate(data.frame(count = PlunkettIDs2017$Binomial), list(value = PlunkettIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Bowman
BowmanIDs2017 <- IDs2017[IDs2017$Site == "Bowman",]

#Determine number of families in "Family" column
BowmanFR2017 <- aggregate(data.frame(count = BowmanIDs2017$Family), list(value = BowmanIDs2017$Family), length)

#Determine number of genera in "Genus" column
BowmanGR2017 <- aggregate(data.frame(count = BowmanIDs2017$Genus), list(value = BowmanIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
BowmanSR2017 <- aggregate(data.frame(count = BowmanIDs2017$Binomial), list(value = BowmanIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Kaldenberg
KaldenbergIDs2017 <- IDs2017[IDs2017$Site == "Kaldenberg",]

#Determine number of families in "Family" column
KaldenbergFR2017 <- aggregate(data.frame(count = KaldenbergIDs2017$Family), list(value = KaldenbergIDs2017$Family), length)

#Determine number of genera in "Genus" column
KaldenbergGR2017 <- aggregate(data.frame(count = KaldenbergIDs2017$Genus), list(value = KaldenbergIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
KaldenbergSR2017 <- aggregate(data.frame(count = KaldenbergIDs2017$Binomial), list(value = KaldenbergIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Kaldenberg
McClellanIDs2017 <- IDs2017[IDs2017$Site == "McClellan",]

#Determine number of families in "Family" column
McClellanFR2017 <- aggregate(data.frame(count = McClellanIDs2017$Family), list(value = McClellanIDs2017$Family), length)

#Determine number of genera in "Genus" column
McClellanGR2017 <- aggregate(data.frame(count = McClellanIDs2017$Genus), list(value = McClellanIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
McClellanSR2017 <- aggregate(data.frame(count = McClellanIDs2017$Binomial), list(value = McClellanIDs2017$Binomial), length)


#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Sloan
SloanIDs2017 <- IDs2017[IDs2017$Site == "Sloan",]

#Determine number of families in "Family" column
SloanFR2017 <- aggregate(data.frame(count = SloanIDs2017$Family), list(value = SloanIDs2017$Family), length)

#Determine number of genera in "Genus" column
SloanGR2017 <- aggregate(data.frame(count = SloanIDs2017$Genus), list(value = SloanIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
SloanSR2017 <- aggregate(data.frame(count = SloanIDs2017$Binomial), list(value = SloanIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Sheller
ShellerIDs2017 <- IDs2017[IDs2017$Site == "Sheller",]

#Determine number of families in "Family" column
ShellerFR2017 <- aggregate(data.frame(count = ShellerIDs2017$Family), list(value = ShellerIDs2017$Family), length)

#Determine number of genera in "Genus" column
ShellerGR2017 <- aggregate(data.frame(count = ShellerIDs2017$Genus), list(value = ShellerIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
ShellerSR2017 <- aggregate(data.frame(count = ShellerIDs2017$Binomial), list(value = ShellerIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Cretsinger
CretsingerIDs2017 <- IDs2017[IDs2017$Site == "Cretsinger",]

#Determine number of families in "Family" column
CretsingerFR2017 <- aggregate(data.frame(count = CretsingerIDs2017$Family), list(value = CretsingerIDs2017$Family), length)

#Determine number of genera in "Genus" column
CretsingerGR2017 <- aggregate(data.frame(count = CretsingerIDs2017$Genus), list(value = CretsingerIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
CretsingerSR2017 <- aggregate(data.frame(count = CretsingerIDs2017$Binomial), list(value = CretsingerIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Subset IDs2017 to show only bees collected from Peckumn
PeckumnIDs2017 <- IDs2017[IDs2017$Site == "Peckumn",]

#Determine number of families in "Family" column
PeckumnFR2017 <- aggregate(data.frame(count = PeckumnIDs2017$Family), list(value = PeckumnIDs2017$Family), length)

#Determine number of genera in "Genus" column
PeckumnGR2017 <- aggregate(data.frame(count = PeckumnIDs2017$Genus), list(value = PeckumnIDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
PeckumnSR2017 <- aggregate(data.frame(count = PeckumnIDs2017$Binomial), list(value = PeckumnIDs2017$Binomial), length)

#-------------------------------------------------------------------#
#                              All Sites                            #
#-------------------------------------------------------------------#
#Determine number of families in "Family" column
IDs2017FR <- aggregate(data.frame(count = IDs2017$Family), list(value = IDs2017$Family), length)

#Determine number of genera in "Genus" column
IDs2017GR <- aggregate(data.frame(count = IDs2017$Genus), list(value = IDs2017$Genus), length)

#Determine number of unique species in "Binomial" column
IDs2017SR <- aggregate(data.frame(count = IDs2017$Binomial), list(value = IDs2017$Binomial), length)

