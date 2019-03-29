#-------------------------------------------------------------------#
#                      Pollen Network Analysis                      #
#-------------------------------------------------------------------#

#Research Question: What does the network between bee species and their collected pollen species look like?

#Objectives:
#Use bipartite package to create a network relating bee species and their associated pollen species

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(dplyr)
library(tidyr)
library(tibble)
library(igraph)
library(bipartite)

#Read in data
Pollen <- read.csv("Pollen/2016-2017 Target Bees and Pollen Reduced.csv", header = TRUE, na.strings = c("", "NA"))

#Reformat Pollen to include bee species in one column and their associated pollen in the next column
pollen <- gather(Pollen, P.ID, Pollen, P1:P25, na.rm = TRUE)

#Filter out "N/A" and wasp entries
pollen <- pollen %>%
  filter(Pollen != "N/A") %>%
  filter(!is.na(Bee.ID)) %>%
  filter(Bee.ID != "Wasp") %>%
  filter(Bee.ID != "Fly")

#Mutate common plant names to species names
pollen <- pollen %>%
  mutate(Pollen.ID = case_when(
    Pollen == "Alfalfa" ~ "Medicago sativa",
    Pollen == "Bee balm" ~ "Monarda fistulosa",
    Pollen == "Birdsfoot trefoil" ~ "Lotus corniculatus",
    Pollen == "Black-eyed Susan" ~ "Rudbeckia hirta",
    Pollen == "Black medic" ~ "Medicago lupulina",
    Pollen == "Bull thistle" ~ "Cirsium vulgare",
    Pollen == "Canada anemone" ~ "Anemone canadensis",
    Pollen == "Canada goldenrod" ~ "Solidago canadensis",
    Pollen == "Canada thistle" ~ "Cirsium arvense",
    Pollen == "Carolina horsenettle" ~ "Solanum carolinense",
    Pollen == "Cleavers" ~ "Galium aparine",
    Pollen == "Common daisy" ~ "Bellis perennis",
    Pollen == "Common daylily" ~ "Hemerocallis fulva",
    Pollen == "Common milkweed" ~ "Asclepias syriaca",
    Pollen == "Common mullein" ~ "Verbascum thapsus",
    Pollen == "Common yellow wood sorrel" ~ "Oxalis stricta",
    Pollen == "Cup plant" ~ "Silphium perfoliatum",
    Pollen == "Curly dock" ~ "Rumex crispus",
    Pollen == "Daisy fleabane" ~ "Erigeron annuus",
    Pollen == "Dandelion" ~ "Taraxacum officinale",
    Pollen == "Deptford pink" ~ "Dianthus armeria",
    Pollen == "Dodder" ~ "Cuscuta gronovii",
    Pollen == "Dogbane" ~ "Apocynum cannabinum",
    Pollen == "Dotted smartweed" ~ "Polygonum punctatum",
    Pollen == "False white indigo" ~ "Baptisia alba",
    Pollen == "Field bindweed" ~ "Convolvulus arvensis",
    Pollen == "Field pennycress" ~ "Thlaspi arvense",
    Pollen == "Golden Alexander" ~ "Zizia aurea",
    Pollen == "Gray-headed coneflower" ~ "Ratibida pinnata",
    Pollen == "Ground cherry" ~ "Physalis virginiana",
    Pollen == "Hairy vetch" ~ "Vicia villosa",
    Pollen == "Hoary vervain" ~ "Verbena stricta",
    Pollen == "Japanese honeysuckle" ~ "Lonicera japonica",
    Pollen == "Marestail" ~ "Conyza canadensis",
    Pollen == "Mock strawberry" ~ "Duchesnea indica",
    Pollen == "Musk thistle" ~ "Carduus nutans",
    Pollen == "Oxeye sunflower" ~ "Heliopsis helianthoides",
    Pollen == "Pennsylvania smartweed" ~ "Polygonum pensylvanicum",
    Pollen == "Pineapple weed" ~ "Matricaria discoidea",
    Pollen == "Prairie ironweed" ~ "Vernonia fasciculata",
    Pollen == "Prickly lettuce" ~ "Lactuca canadensis",
    Pollen == "Purple coneflower" ~ "Echinacea purpurea",
    Pollen == "Purple prairie clover" ~ "Dalea purpurea",
    Pollen == "Queen Anne's lace" ~ "Daucus carota",
    Pollen == "Rattlesnake master" ~ "Eryngium yuccifolium",
    Pollen == "Red clover" ~ "Trifolium pratense",
    Pollen == "Red raspberry" ~ "Rubus idaeus",
    Pollen == "Sawtooth sunflower" ~ "Helianthus grosseserratus",
    Pollen == "Showy tick trefoil" ~ "Desmodium canadense",
    Pollen == "Soapwort" ~ "Saponaria officinalis",
    Pollen == "Sow thistle" ~ "Sonchus arvensis",
    Pollen == "Star of Bethlehem" ~ "Ornithogalum umbellatum",
    Pollen == "Stiff goldenrod" ~ "Solidago rigida",
    Pollen == "Velvet leaf" ~ "Abutilon theophrasti",
    Pollen == "White campion" ~ "Silene latifolia",
    Pollen == "White clover" ~ "Trifolium repens",
    Pollen == "White sweet clover" ~ "Melilotus albus",
    Pollen == "Whorled milkweed" ~ "Asclepias verticillata",
    Pollen == "Wild cucumber" ~ "Echinocystis lobata",
    Pollen == "Wild mustard" ~ "Sinapis arvensis",
    Pollen == "Wild parsnip" ~ "Pastinaca sativa",
    Pollen == "Yarrow" ~ "Achillea millefolium",
    Pollen == "Yellow sweet clover" ~ "Melilotus officinalis"
  ))

#Check pollen names to be sure they're consistent
pollen.table <- pollen %>%
  group_by(Pollen.ID) %>%
  tally()

#Check bee names to be sure they're consistent
bee.table <- Pollen %>%
  filter(!is.na(Bee.ID)) %>%
  group_by(Bee.ID) %>%
  tally()

#Filter out entries with duplicate bee/pollen values by date
pollen.nodupes <- pollen %>%
  group_by(Site, Date.Collected) %>%
  distinct(Bee.ID, Pollen.ID)

#Make a table showing the number of bee species were found to be using each pollen species
pollen.bees <- pollen.nodupes %>%
  group_by(Pollen.ID) %>%
  summarise(no.beespp = (n_distinct(Bee.ID)))

#Produce table with the number of instances each bee species has collected each pollen species
bees.pollen <- pollen.nodupes %>%
  group_by(Bee.ID) %>%
  count(Pollen.ID)

#Table showing the number of instances each pollen species was identified
pollen.interactions <- bees.pollen %>%
  group_by(Pollen.ID) %>%
  summarise(sum(n))

#Format from long to wide using spread
bees.pollen <- spread(bees.pollen, Pollen.ID, n)

#Remove bee species names from first column and move to row names
bees.pollen <- column_to_rownames(bees.pollen, "Bee.ID")

#Fill NAs with 0
bees.pollen[is.na(bees.pollen)] <- 0

#Make font italic
par(font = 3)

#Graph using bipartite package
plotweb(bees.pollen,
        abuns.type = "additional",
        col.interaction = t(ifelse(bees.pollen[,] > 5,
                                   adjustcolor("black", alpha.f = 0.5),
                                   adjustcolor("grey80", alpha.f = 0.5))),
        text.rot = 90,
        y.lim = c(-1, 2.5),
        col.low = "red",
        col.high = "yellow",
        bor.col.interaction = NA)

#Gross code ####
#Keep only Bee.ID and Pollen columns
#reduced.pollennodupes <- subset(pollen.nodupes, select = c(Bee.ID, Pollen))

#Change reduced.pollen to a graph.data.frame
reducedpollennodupes.graph <- graph.data.frame(reduced.pollennodupes, directed = FALSE)

#View bipartite.mapping modes
bipartite.mapping(reducedpollennodupes.graph)

#Add bipartite.mapping type to data frame
V(reducedpollennodupes.graph)$type <- bipartite.mapping(reducedpollennodupes.graph)$type

#Graph it
V(reducedpollennodupes.graph)$color <- ifelse(V(reducedpollennodupes.graph)$type, "lightblue", "salmon")
V(reducedpollennodupes.graph)$shape <- ifelse(V(reducedpollennodupes.graph)$type, "circle", "square")
plot(reducedpollennodupes.graph, vertex.label.cex = 0.7, vertex.label.color = "black", vertex.size = 7)
plot(reducedpollennodupes.graph, layout = layout.bipartite, vertex.label.cex = 0.7, vertex.label.color = "black", vertex.size = 7)