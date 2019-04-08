#-------------------------------------------------------------------#
#                  Summary Statistics for Full Dataset              #
#                              2014-2018                            #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(lubridate)
library(dplyr)
library(tidyr)

#Read in data
Bees <- read.csv("Bees/Bee IDs.csv", header = T, na.strings = c("", "NA"))
Quadrats <- read.csv("Plants/Quadrats.csv")

#Format date with lubridate
Bees$Date <- mdy(Bees$Date)
Quadrats$Date <- mdy(Quadrats$Date)

#Change Year from number to year
Bees$Year <- year(Bees$Date)
Quadrats$Year <- year(Quadrats$Date)

#Filter out bad stuff from Bees
bees <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(Binomial != "Wasp") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site))

#Format Bare.Ground column as numeric
Quadrats$BareGround <- as.numeric(Quadrats$BareGround)

#Average bare ground cover for each site
bareground <- Quadrats %>%
  filter(!is.na(BareGround)) %>%
  group_by(Site, Date, Quadrat) %>%
  summarise(total.bareground = BareGround[1])
avg.bareground <- bareground %>%
  group_by(Site) %>%
  summarise(avg.bareground = mean(total.bareground), 
            number.quadrats = length(total.bareground))

#Average floral cover for each site
floral.cover <- Quadrats %>%
  group_by(Site) %>%
  summarise(avg.floralcover = mean(Cover))

#Determine species richness without nesting plots
beespp.noplot <- Bees %>%
  filter(!is.na(Binomial)) %>%
  filter(Family != "Wasp") %>%
  filter(Trap != "Plot") %>%
  count(Binomial)

#Bee species richness
beespp <- bees %>%
  count(Binomial)

#Export as .csv
#write.csv(beespp, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/SummaryStats/BeeSpeciesRichness.csv")

#Determine abundance for each site
bees.site <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(!is.na(Binomial)) %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  count(Binomial)
bees.site <- bees.site %>%
  group_by(Site) %>%
  summarise(total.bees = sum(n))

#Check to make sure total matches original datafile
bees.site %>%
  summarise(sum(total.bees))
#15,904, good to go!

#Determine abundance by trap for each site/date
AbundTrap <- Bees %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date, Trap) %>%
  count(Binomial)
AbundTrap <- AbundTrap %>%
  group_by(Site, Date, Trap) %>%
  summarise(Bee.Abundance = sum(n))
AbundTrapwide <- spread(AbundTrap, Trap, Bee.Abundance)

#Export as .csv
#write.csv(AbundTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/AbundancebyTrap1234.csv")

#Determine species richness for each site
beespp.site <- Bees %>%
  filter(Family != "Wasp") %>%
  filter(Family != "Fly") %>%
  filter(Binomial != "Unidentifiable") %>%
  filter(!is.na(Site)) %>%
  group_by(Site) %>%
  summarise(no.beespp = n_distinct(Binomial))

#Determine species richness by trap for each site/date
SpecRichTrap <- Bees %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date, Trap) %>%
  summarise(Bee.Species.Richness = length(unique(Binomial)))
SpecRichTrapwide <- spread(SpecRichTrap, Trap, Bee.Species.Richness)

#Export as .csv
#write.csv(SpecRichTrapwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/SpeciesRichnessbyTrap1234.csv")

#Determine genus richness for each site/date
GenusRich <- Bees %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  summarise(Bee.Genus.Richness = length(unique(Genus)))

#Export as .csv
#write.csv(GenusRich, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/GenusRichness1234.csv")

#Determine number of individuals of each species collected for each site/date
SpecRichAbund <- Bees %>%
  filter(Trap != "Target") %>%
  filter(Family != "Wasp") %>%
  group_by(Site, Date) %>%
  count(Binomial)
SpecRichAbundwide <- spread(SpecRichAbund, Binomial, n)
SpecRichAbundwide[is.na(SpecRichAbundwide)] <- 0

#Export as .csv
#write.csv(SpecRichAbundwide, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/SummaryStats/SpeciesRichnessandAbundance1234.csv")

#Genus richness for 2014-2018
genusrich <- Bees %>%
  group_by(Genus) %>%
  count(Genus)

#Determine the number of individuals collected in nesting plots by year
npbees <- bees %>%
  filter(Trap == "Plot") %>%
  group_by(Site, Year) %>%
  count(Binomial)
npbees <- npbees %>%
  summarise(no.bees = sum(n))

#Determine the number of species collected in nesting plots by year
npbeespp <- bees %>%
  filter(Trap == "Plot") %>%
  group_by(Site, Year) %>%
  summarise(no.npbeespp = n_distinct(Binomial))

#Explore number of bees and number of species collected in emergence traps by year to check for differences in emergence trap deployment timing (2014 in late May, 2015-2016 in early May, 2017-2018 in late April)
etrap.bees <- bees %>%
  filter(Trap == "Emergence") %>%
  group_by(Date) %>%
  count(Binomial)

#Format from long to wide for easier interpretation
etrap.bees.wide <- spread(etrap.bees, Date, n)

#Export as .csv
#write.csv(etrap.bees.wide, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/SummaryStats/EmergenceTrapBeesbyDate.csv")

#Floral resources
#Determine relative abundance of each floral species during each sampling event
Quadrats <- read.csv("Plants/Quadrats.csv", na.strings = c("", "NA"))
Quadrats$Date <- mdy(Quadrats$Date)

#Filter 2016-2017
quads34 <- Quadrats %>%
  filter(Year == "3" | Year == "4")

#Mutate flower names
quads34 <- quads34 %>%
  mutate(Species = case_when(
    Species == "Alfalfa" ~ "Medicago sativa",
    Species == "Bee balm" ~ "Monarda fistulosa",
    Species == "Birdsfoot trefoil" ~ "Lotus corniculatus",
    Species == "Black-eyed Susan" ~ "Rudbeckia hirta",
    Species == "Black medic" ~ "Medicago lupulina",
    Species == "Bull thistle" ~ "Cirsium vulgare",
    Species == "Canada anemone" ~ "Anemone canadensis",
    Species == "Canada goldenrod" ~ "Solidago canadensis",
    Species == "Canada thistle" ~ "Cirsium arvense",
    Species == "Carolina horsenettle" ~ "Solanum carolinense",
    Species == "Cleavers" ~ "Galium aparine",
    Species == "Common daisy" ~ "Bellis perennis",
    Species == "Common daylily" ~ "Hemerocallis fulva",
    Species == "Common milkweed" ~ "Asclepias syriaca",
    Species == "Common mullein" ~ "Verbascum thapsus",
    Species == "Common yellow wood sorrel" ~ "Oxalis stricta",
    Species == "Cup plant" ~ "Silphium perfoliatum",
    Species == "Curly dock" ~ "Rumex crispus",
    Species == "Daisy fleabane" ~ "Erigeron annuus",
    Species == "Dandelion" ~ "Taraxacum officinale",
    Species == "Deptford pink" ~ "Dianthus armeria",
    Species == "Dodder" ~ "Cuscuta gronovii",
    Species == "Dogbane" ~ "Apocynum cannabinum",
    Species == "Dotted smartweed" ~ "Polygonum punctatum",
    Species == "False white indigo" ~ "Baptisia alba",
    Species == "Field bindweed" ~ "Convolvulus arvensis",
    Species == "Field pennycress" ~ "Thlaspi arvense",
    Species == "Golden Alexander" ~ "Zizia aurea",
    Species == "Gray-headed coneflower" ~ "Ratibida pinnata",
    Species == "Ground cherry" ~ "Physalis virginiana",
    Species == "Hairy vetch" ~ "Vicia villosa",
    Species == "Hoary vervain" ~ "Verbena stricta",
    Species == "Japanese honeysuckle" ~ "Lonicera japonica",
    Species == "Marestail" ~ "Conyza canadensis",
    Species == "Mock strawberry" ~ "Duchesnea indica",
    Species == "Musk thistle" ~ "Carduus nutans",
    Species == "Oxeye sunflower" ~ "Heliopsis helianthoides",
    Species == "Pennsylvania smartweed" ~ "Polygonum pensylvanicum",
    Species == "Pineapple weed" ~ "Matricaria discoidea",
    Species == "Prairie ironweed" ~ "Vernonia fasciculata",
    Species == "Prickly lettuce" ~ "Lactuca canadensis",
    Species == "Purple coneflower" ~ "Echinacea purpurea",
    Species == "Purple prairie clover" ~ "Dalea purpurea",
    Species == "Queen Anne's lace" ~ "Daucus carota",
    Species == "Rattlesnake master" ~ "Eryngium yuccifolium",
    Species == "Red clover" ~ "Trifolium pratense",
    Species == "Red raspberry" ~ "Rubus idaeus",
    Species == "Sawtooth sunflower" ~ "Helianthus grosseserratus",
    Species == "Showy tick trefoil" ~ "Desmodium canadense",
    Species == "Soapwort" ~ "Saponaria officinalis",
    Species == "Sow thistle" ~ "Sonchus arvensis",
    Species == "Star of Bethlehem" ~ "Ornithogalum umbellatum",
    Species == "Stiff goldenrod" ~ "Solidago rigida",
    Species == "Velvet leaf" ~ "Abutilon theophrasti",
    Species == "White campion" ~ "Silene latifolia",
    Species == "White clover" ~ "Trifolium repens",
    Species == "White sweet clover" ~ "Melilotus albus",
    Species == "Whorled milkweed" ~ "Asclepias verticillata",
    Species == "Wild cucumber" ~ "Echinocystis lobata",
    Species == "Wild mustard" ~ "Sinapis arvensis",
    Species == "Wild parsnip" ~ "Pastinaca sativa",
    Species == "Yarrow" ~ "Achillea millefolium",
    Species == "Yellow sweet clover" ~ "Melilotus officinalis"
  ))

#Determine total number of plants in bloom in quadrats during 2016-2017
bloom.plants <- quads34 %>%
  filter(!is.na(Species)) %>%
  group_by(Species) %>%
  count()

#Convert percent coverage to square meters
plantcover.sqm <- quads34 %>%
  filter(!is.na(Species)) %>%
  mutate(cover.sqm = Cover/100)

#Calculate total amount of coverage for each species
plantcover.sqm <- plantcover.sqm %>%
  group_by(Species) %>%
  summarise(Total.Cover = sum(cover.sqm))

#Divide Total Cover by 800 sq. m. to determine total relative abundance of each floral species over 2016-2017
plantcover.sqm <- plantcover.sqm %>%
  group_by(Species) %>%
  mutate(rel.abun = Total.Cover/800)

#Determine percentage of relative abundance
plantcover.sqm <- plantcover.sqm %>%
  group_by(Species) %>%
  mutate(rel.abun.per = rel.abun*100)

#Old code ####
#Reformat dataset from long to wide
quads34.wide <- spread(quads34.long, Species, Total.Cover)

#Fill NAs with 0
quads34.wide[is.na(quads34.wide)] <- 0

#Remove V1 column
quads34.wide <- quads34.wide %>%
  select(-V1)

#Combine Site and Date columns
quads34.wide <- quads34.wide %>%
  unite(SiteDate, c(Site, Date), sep = " ", remove = TRUE)

#Change column to rownames
quads34.wide <- quads34.wide %>%
  remove_rownames %>%
  column_to_rownames(var = "SiteDate")

#Determine relative abundance of blooming plants in quadrats with vegan
quads.rel <- decostand(quads34.wide, method = "total", na.rm = FALSE)

#Export as .csv
write.csv(quads.rel, file = "C:/Users/Morgan Mackert/Documents/ISU/Project/mmackert/Graphs/SummaryStats/Plant Abundance.csv")
