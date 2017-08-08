#####################################################################
#                         LAND USE GRAPHS                           #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
library(ggplot2)
library(plotly)
library(dplyr)

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
PLlandusepoint <- ggplot(data = PlunkettLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nPlunkett (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PLlandusepoint

PLlandusebar <- ggplot(data = PlunkettLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
PLlandusebar

#Create pie chart using Plotly
#Establish color scheme for pie chart
colors <- c('white', 'yellow', 'darkgreen', 'violet', 'gray', 'saddlebrown', 'olivedrab')
#Make the graph
PLlandusepie <- plot_ly(PlunkettLandUse, labels = PlunkettLandUse$LandType, values = PlunkettLandUse$Coverage, type = 'pie',
             textposition = 'outside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Plunkett (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
PLlandusepie

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Read in data
BowmanLandUse <- read.csv("sites/BowmanLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
BowmanLandUse$LandType <- as.character(BowmanLandUse$LandType)
BowmanLandUse$Coverage <- as.numeric(BowmanLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
BowmanLandUse$Proportions <- BowmanLandUse$Coverage/sum(BowmanLandUse$Coverage)*100

#Create graphs
BOlandusepoint <- ggplot(data = BowmanLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nBowman (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
BOlandusepoint

BOlandusebar <- ggplot(data = BowmanLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
BOlandusebar

#Create pie chart using Plotly
BOlandusepie <- plot_ly(BowmanLandUse, labels = BowmanLandUse$LandType, values = BowmanLandUse$Coverage, type = 'pie',
                      textposition = 'outside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      marker = list(colors = colors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Bowman (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
BOlandusepie

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Read in data
KaldenbergLandUse <- read.csv("sites/KaldenbergLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
KaldenbergLandUse$LandType <- as.character(KaldenbergLandUse$LandType)
KaldenbergLandUse$Coverage <- as.numeric(KaldenbergLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
KaldenbergLandUse$Proportions <- KaldenbergLandUse$Coverage/sum(KaldenbergLandUse$Coverage)*100

#Create graphs
KAlandusepoint <- ggplot(data = KaldenbergLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nKaldenberg (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
KAlandusepoint

KAlandusebar <- ggplot(data = KaldenbergLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
KAlandusebar

#Create pie chart using Plotly
KAlandusepie <- plot_ly(KaldenbergLandUse, labels = KaldenbergLandUse$LandType, values = KaldenbergLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Kaldenberg (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
KAlandusepie

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Read in data
McClellanLandUse <- read.csv("sites/McClellanLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
McClellanLandUse$LandType <- as.character(McClellanLandUse$LandType)
McClellanLandUse$Coverage <- as.numeric(McClellanLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
McClellanLandUse$Proportions <- McClellanLandUse$Coverage/sum(McClellanLandUse$Coverage)*100

#Create graphs
MClandusepoint <- ggplot(data = McClellanLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nMcClellan (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
MClandusepoint

MClandusebar <- ggplot(data = McClellanLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
MClandusebar

#Create pie chart using Plotly
MClandusepie <- plot_ly(McClellanLandUse, labels = McClellanLandUse$LandType, values = McClellanLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding McClellan (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
MClandusepie

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Read in data
SloanLandUse <- read.csv("sites/SloanLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
SloanLandUse$LandType <- as.character(SloanLandUse$LandType)
SloanLandUse$Coverage <- as.numeric(SloanLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
SloanLandUse$Proportions <- SloanLandUse$Coverage/sum(SloanLandUse$Coverage)*100

#Create graphs
SLlandusepoint <- ggplot(data = SloanLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nSloan (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
SLlandusepoint

SLlandusebar <- ggplot(data = SloanLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
SLlandusebar

#Create pie chart using Plotly
SLlandusepie <- plot_ly(SloanLandUse, labels = SloanLandUse$LandType, values = SloanLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Sloan (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
SLlandusepie

#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Read in data
ShellerLandUse <- read.csv("sites/ShellerLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
ShellerLandUse$LandType <- as.character(ShellerLandUse$LandType)
ShellerLandUse$Coverage <- as.numeric(ShellerLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
ShellerLandUse$Proportions <- ShellerLandUse$Coverage/sum(ShellerLandUse$Coverage)*100

#Create graphs
SHlandusepoint <- ggplot(data = ShellerLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nSheller (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
SHlandusepoint

SHlandusebar <- ggplot(data = ShellerLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
SHlandusebar

#Create pie chart using Plotly
SHlandusepie <- plot_ly(ShellerLandUse, labels = ShellerLandUse$LandType, values = ShellerLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Sheller (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
SHlandusepie

#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Read in data
CretsingerLandUse <- read.csv("sites/CretsingerLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
CretsingerLandUse$LandType <- as.character(CretsingerLandUse$LandType)
CretsingerLandUse$Coverage <- as.numeric(CretsingerLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
CretsingerLandUse$Proportions <- CretsingerLandUse$Coverage/sum(CretsingerLandUse$Coverage)*100

#Create graphs
CRlandusepoint <- ggplot(data = CretsingerLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nCretsinger (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
CRlandusepoint

CRlandusebar <- ggplot(data = CretsingerLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
CRlandusebar

#Create pie chart using Plotly
CRlandusepie <- plot_ly(CretsingerLandUse, labels = CretsingerLandUse$LandType, values = CretsingerLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Cretsinger (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
CRlandusepie

#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Read in data
PeckumnLandUse <- read.csv("sites/PeckumnLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
PeckumnLandUse$LandType <- as.character(PeckumnLandUse$LandType)
PeckumnLandUse$Coverage <- as.numeric(PeckumnLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
PeckumnLandUse$Proportions <- PeckumnLandUse$Coverage/sum(PeckumnLandUse$Coverage)*100

#Create graphs
PElandusepoint <- ggplot(data = PeckumnLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nPeckumn (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
PElandusepoint

PElandusebar <- ggplot(data = PeckumnLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
PElandusebar

#Create pie chart using Plotly
PElandusepie <- plot_ly(PeckumnLandUse, labels = PeckumnLandUse$LandType, values = PeckumnLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Peckumn (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
PElandusepie

#-------------------------------------------------------------------#
#                        Stacked Bar Graph                          #
#-------------------------------------------------------------------#
#Read in data
FullLandUse <- read.csv("sites/FullLandUse.csv")

#Figure out proportions of each land type per each site
#Determine sum of land cover for each site 
SumLandUse <- FullLandUse %>%
  group_by(Site) %>%
  summarise(TotalCoverage = sum(Coverage))

#Join these two datasets together
FullLandUseJoined <- full_join(FullLandUse, SumLandUse, by = c("Site")) 

#Determine proportions using new summed values
FullLandUseJoined$Proportion <- (FullLandUseJoined$Coverage/FullLandUseJoined$TotalCoverage)*100

#Define specific colors for each land type
barcolors <- c("Undefined" = "white", "Corn" = "yellow", "Soybeans" = "darkgreen", "Alfalfa" = "violet", "Developed" = "gray", "Deciduous Forest" = "saddlebrown", "Grass/Pasture" = "olivedrab")

#Make graph of Land Coverage (km^2)
Fulllandusebarcover <- ggplot(FullLandUseJoined, aes(x = Site, y = Coverage, fill = LandType)) +
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Land Use Surrounding Each Site \nWithin a 3km Radius") +
  theme_bw() +
  scale_fill_manual(values = barcolors) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coverage (sq. km)") +
  labs(fill = "Land Type")
Fulllandusebarcover

#Make graph of Land Coverage (%)
Fulllandusebarprop <- ggplot(FullLandUseJoined, aes(x = Site, y = Proportion, fill = LandType)) +
  geom_bar(stat = "identity", color = "black") +
  ggtitle("Land Use Surrounding Each Site \nWithin a 3km Radius") +
  theme_bw() +
  scale_fill_manual(values = barcolors) +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coverage (%)") +
  labs(fill = "Land Type")
Fulllandusebarprop
