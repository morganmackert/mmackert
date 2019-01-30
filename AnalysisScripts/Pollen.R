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
library(lubridate)
library(dplyr)
library(tidyr)
library(bipartite)

#Read in data
Pollen <- read.csv("Pollen/2016-2017 Target Bees and Pollen.csv", header = TRUE, na.strings = c("", "NA"))

#Reformat Pollen to include bee species in one column and their associated pollen in the next column
pollen <- gather(Pollen, P.ID, Pollen, P1:P25, na.rm = TRUE)

#Filter out "N/A" entries
pollen <- pollen %>%
  filter(Pollen != "N/A") %>%
  filter(!is.na(Bee.ID)) %>%
  filter(Bee.ID != "Wasp")

#Check pollen names to be sure they're consistent
pollen.table <- pollen %>%
  group_by(Pollen) %>%
  tally()

#Keep only Bee.ID and Pollen columns
reduced.pollen <- subset(pollen, select = c(Bee.ID, Pollen))

#Change reduced.pollen to a graph.data.frame
reducedpollen.graph <- graph.data.frame(reduced.pollen, directed = FALSE)

#View bipartite.mapping modes
bipartite.mapping(reducedpollen.graph)

#Add bipartite.mapping type to data frame
V(reducedpollen.graph)$type <- bipartite.mapping(reducedpollen.graph)$type

#Graph it
plot(reducedpollen.graph, layout = layout.bipartite)

     