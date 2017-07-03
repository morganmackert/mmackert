#####################################################################
#                         LAND USE GRAPHS                           #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
library(ggplot2)
library(plotly)

#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#

#Read in data
PlunkettLandUse <- read.csv("sites/PlunkettLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
PlunkettLandUse$LandType <- as.character(PlunkettLandUse$LandType)
PlunkettLandUse$Coverage <- as.numeric(PlunkettLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
PlunkettLandUse$Proportions <- PlunkettLandUse$Coverage/sum(PlunkettLandUse$Coverage)*100

#Create graphs
landusepoint <- ggplot(data = PlunkettLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nPlunkett (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
landusepoint

landusebar <- ggplot(data = PlunkettLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")

#Create pie chart using Plotly
#Establish color scheme for pie chart
colors <- c('white', 'yellow', 'green', 'violet', 'gray', 'saddlebrown', 'olivedrab')
#Make the graph
landusepie <- plot_ly(PlunkettLandUse, labels = PlunkettLandUse$LandType, values = PlunkettLandUse$Coverage, type = 'pie',
             textposition = 'outside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Plunkett (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
landusepie

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#