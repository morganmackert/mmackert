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
Pollen <- read.csv("Pollen/2016-2017 Target Bees and Pollen.csv", header = TRUE, na.strings = c("", "NA"))

#Reformat Pollen to include bee species in one column and their associated pollen in the next column
pollen <- gather(Pollen, P.ID, Pollen, P1:P25, na.rm = TRUE)

#Filter out "N/A" and wasp entries
pollen <- pollen %>%
  filter(Pollen != "N/A") %>%
  filter(!is.na(Bee.ID)) %>%
  filter(Bee.ID != "Wasp")

#Check pollen names to be sure they're consistent
pollen.table <- pollen %>%
  group_by(Pollen) %>%
  tally()

#Check bee names to be sure they're consistent
bee.table <- pollen %>%
  group_by(Bee.ID) %>%
  tally()

#Filter out entries with duplicate bee/pollen values by date
pollen.nodupes <- pollen %>%
  group_by(Date.Collected) %>%
  distinct(Bee.ID, Pollen)

#Produce table with the number of instances each bee species has collected each pollen species
bees.pollen <- pollen.nodupes %>%
  group_by(Bee.ID) %>%
  count(Pollen)

#Format from long to wide using spread
bees.pollen <- spread(bees.pollen, Pollen, n)

#Remove bee species names from first column and move to row names
bees.pollen <- column_to_rownames(bees.pollen, "Bee.ID")

#Fill NAs with 0
bees.pollen[is.na(bees.pollen)] <- 0

#Graph using bipartite package
plotweb(bees.pollen,
        abuns.type = "additional",
        col.interaction = t(ifelse(bees.pollen[,] > 5,
                                   adjustcolor("black", alpha.f = 0.5),
                                   adjustcolor("grey80", alpha.f = 0.5))),
        text.rot = 90,
        col.low = "red",
        col.high = "yellow",
        bor.col.interaction = NA)

visweb(bees.pollen)

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