#-------------------------------------------------------------------#
#                          Land Use Graphs                          #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Data")

#Load libraries
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(vegan)

#Plunkett ####
#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Read in data
PlunkettLandUse <- read.csv("Sites/PlunkettLandUse.csv")

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

#Bowman ####
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

#Kaldenberg ####
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

#McClellan ####
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

#Sloan ####
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

#Sheller ####
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

#Cretsinger ####
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

#Peckumn ####
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

#Elkader ####
#-------------------------------------------------------------------#
#                              Elkader                              #
#-------------------------------------------------------------------#
#Read in data
ElkaderLandUse <- read.csv("sites/ElkaderLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
ElkaderLandUse$LandType <- as.character(ElkaderLandUse$LandType)
ElkaderLandUse$Coverage <- as.numeric(ElkaderLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
ElkaderLandUse$Proportions <- ElkaderLandUse$Coverage/sum(ElkaderLandUse$Coverage)*100

#Create graphs
ELlandusepoint <- ggplot(data = ElkaderLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nElkader (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ELlandusepoint

ELlandusebar <- ggplot(data = ElkaderLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
ELlandusebar

#Create pie chart using Plotly
ELlandusepie <- plot_ly(ElkaderLandUse, labels = ElkaderLandUse$LandType, values = ElkaderLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Elkader (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
ELlandusepie

#Greving ####
#-------------------------------------------------------------------#
#                              Greving                              #
#-------------------------------------------------------------------#
#Read in data
GrevingLandUse <- read.csv("sites/GrevingLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
GrevingLandUse$LandType <- as.character(GrevingLandUse$LandType)
GrevingLandUse$Coverage <- as.numeric(GrevingLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
GrevingLandUse$Proportions <- GrevingLandUse$Coverage/sum(GrevingLandUse$Coverage)*100

#Create graphs
GRlandusepoint <- ggplot(data = GrevingLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nGreving (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
GRlandusepoint

GRlandusebar <- ggplot(data = GrevingLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
GRlandusebar

#Create pie chart using Plotly
GRlandusepie <- plot_ly(GrevingLandUse, labels = GrevingLandUse$LandType, values = GrevingLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Greving (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
GRlandusepie

#Neal Smith ####
#-------------------------------------------------------------------#
#                           Neal Smith                              #
#-------------------------------------------------------------------#
#Read in data
NealSmithLandUse <- read.csv("sites/NealSmithLandUse.csv")

#Change Land Type column to "character" and Coverage column to "numeric"
NealSmithLandUse$LandType <- as.character(NealSmithLandUse$LandType)
NealSmithLandUse$Coverage <- as.numeric(NealSmithLandUse$Coverage)

#Determine proportions of land coverage rather than raw values
NealSmithLandUse$Proportions <- NealSmithLandUse$Coverage/sum(NealSmithLandUse$Coverage)*100

#Create graphs
NSlandusepoint <- ggplot(data = NealSmithLandUse, aes(x = LandType, y = Proportions)) +
  geom_point() +
  labs(x = "Land Cover Type", y = "Percent Coverage") +
  ggtitle("Land Coverage Surrounding \nNealSmith (3 km)") +
  theme_bw() +
  theme(plot.title= element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
NSlandusepoint

NSlandusebar <- ggplot(data = NealSmithLandUse, aes(x = LandType, y = Coverage)) +
  geom_bar(stat = "identity")
NSlandusebar

#Create pie chart using Plotly
NSlandusepie <- plot_ly(NealSmithLandUse, labels = NealSmithLandUse$LandType, values = NealSmithLandUse$Coverage, type = 'pie',
                        textposition = 'outside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title = 'Land Coverage Surrounding Neal Smith (3 km)',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
NSlandusepie

#All Sites ####
#-------------------------------------------------------------------#
#                        Stacked Bar Graph                          #
#-------------------------------------------------------------------#
#Read in data
FullLandUse <- read.csv("Sites/FullLandUse.csv")

#Assign number to each site
FullLandUse <- FullLandUse %>%
  mutate(SiteNum = ifelse(Site == "Bowman", "1",
                          ifelse(Site == "Cretsinger", "2",
                                 ifelse(Site == "Elkader", "3",
                                        ifelse(Site == "Greving", "4",
                                               ifelse(Site == "Kaldenberg", "5",
                                                      ifelse(Site == "McClellan", "6",
                                                             ifelse(Site == "NealSmith", "7",
                                                                    ifelse(Site == "Peckumn", "8",
                                                                           ifelse(Site == "Plunkett", "9",
                                                                                  ifelse(Site == "Sheller", "10",
                                                                                         ifelse(Site == "Sloan", "11",
                                                                                                NA
                                                                                                ))))))))))))


#Figure out proportions of each land type per each site
#Determine sum of land cover for each site 
SumLandUse <- FullLandUse %>%
  group_by(SiteNum) %>%
  summarise(TotalCoverage = sum(Coverage))

#Join these two datasets together
FullLandUseJoined <- full_join(FullLandUse, SumLandUse, by = c("SiteNum")) 

#Determine proportions using new summed values
FullLandUseJoined$Proportion <- (FullLandUseJoined$Coverage/FullLandUseJoined$TotalCoverage)*100

#Define specific colors for each land type
barcolors <- c("Undefined" = "white", "Corn" = "yellow", "Soybeans" = "darkgreen", "Alfalfa" = "violet", "Developed Land" = "darkgray", "Deciduous Forest" = "saddlebrown", "Grass/Pasture" = "olivedrab")

#Make graph of Land Coverage (km^2)
Fulllandusebarcover <- ggplot(FullLandUseJoined,
                              aes(x = SiteNum,
                                  y = Coverage,
                                  fill = LandType)) +
  geom_bar(stat = "identity",
           color = "black") +
  ggtitle("Land Use Surrounding Each Site \nWithin a 3km Radius") +
  theme_bw() +
  scale_fill_manual(values = barcolors) +
  scale_x_discrete(limits = c("6", "1", "10", "3", "4", "11", "5", "7", "9", "2", "8")) +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.title.align = 0.5) +
  labs(x = "Site Number",
       y = "Coverage (sq. km)",
       fill = "Land Type")
Fulllandusebarcover

#Make graph of Land Coverage (%)
Fulllandusebarprop <- ggplot(FullLandUseJoined,
                             aes(x = SiteNum,
                                 y = Proportion,
                                 fill = LandType)) +
  geom_bar(stat = "identity",
           color = "black") +
  ggtitle("Land Use Surrounding Each Site \nWithin a 3km Radius") +
  theme_bw() +
  scale_fill_manual(values = barcolors) +
  scale_x_discrete(limits = c("6", "1", "10", "3", "4", "11", "5", "7", "9", "2", "8")) +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(hjust = 1)) +
  theme(legend.title.align = 0.5) +
  labs(x = "Site Number",
       y = "Coverage (%)",
       fill = "Land Type")
Fulllandusebarprop

#-------------------------------------------------------------------#
#                                PCA                                #
#-------------------------------------------------------------------#
landpca <- princomp(FullLandUse.wide %>% select(-Site, -SiteNum))
print(landpca$loadings)
biplot(landpca)
#Use cor=TRUE when variables are different scales. Our scales are all the same, so no need to do this.

#Old code ####
#-------------------------------------------------------------------#
#                            Land Use MRPP                          #
#-------------------------------------------------------------------#
#Format FullLandUse from long to wide
FullLandUse.wide <- spread(FullLandUse, LandType, Coverage)

#Move Site column to another data frame
FullLandUse.widesites <- FullLandUse.wide["Site"]

#Remove Site and SiteNum columns
FullLandUse.wide <- FullLandUse.wide[!names(FullLandUse.wide) %in% c("Site", "SiteNum")]

#Convert to a data.frame
FullLandUse.wide <- as.data.frame(FullLandUse.wide)

#Perform MRPP analysis
landuse.mrpp <- mrpp(FullLandUse.wide, FullLandUse.widesites, distance = "bray")
landuse.mrpp

#-------------------------------------------------------------------#
#                     Land Use ANOVA and ANOSIM                     #
#-------------------------------------------------------------------#
#Distance measures of LandUse data frame
land.dist <- vegdist(FullLandUse.wide)

#Attach site names
attach(FullLandUse.widesites)

#ANOSIM test
land.ano <- anosim(land.dist, Site)
summary(land.ano)

#ANOVA
land.aov <- aov(Coverage ~ Site, data = FullLandUse)
land.lm <- lm(Coverage ~ Site, data = FullLandUse)
summary(land.aov)
summary(land.lm)
head(FullLandUse)

ggplot(FullLandUse, aes(x = Site, y = Coverage)) +
  geom_boxplot()

anova(land.lm)
str(FullLandUse)
