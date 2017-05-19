#####################################################################
#                              BAR GRAPHS                           #
#####################################################################

#Set working directory
setwd("~/ISU/Project/Previous Data/Data Files")

#Load libraries
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

#####What does "Measure" column mean? Is that the average bee abundance for each category?
#####How did you calculate each column?
#####What does this look like using plant diversity as a continuous variable rather than a categorical variable?

#-------------------------------------------------------------------#
#                  Plant Diversity ~ Bee Abundance                  #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
bm <- read.csv("Bee_BarGraph_Abundance.csv")

#PlantDiversity column in "bm" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
bm$PlantDiversity <- factor(bm$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonBAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Bee Abundance
p<-ggplot(data=bm,aes(x=factor(PlantDiversity),y=Measure))
p+geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonBAlimits,position=position_dodge(0.9),width=0.25)+
  geom_text(aes(x=factor(PlantDiversity),y=c(180,300,460),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Morgan's plot: Plant Diversity vs. Bee Abundance
PDonBA <- ggplot(data = bm, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonBAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(183,298,462), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nTotal Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonBA

#-------------------------------------------------------------------#
#                    Plant Diversity ~ Bee Richness                 #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
br <- read.csv("Bee_BarGraph_Richness.csv")

#PlantDiversity column in "br" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
br$PlantDiversity <- factor(br$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonBRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Bee Richness
p<-ggplot(data=br,aes(x=factor(PlantDiversity),y=Measure))
p+geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Species Richness")+
  geom_errorbar(PDonBRlimits,position=position_dodge(0.9),width=0.25)+
  geom_text(aes(x=factor(PlantDiversity),y=c(22,41,45),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Morgan's plot: Plant Diversity vs. Bee Richness
PDonBR <- ggplot(data = br, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Species Richness") +
  geom_errorbar(PDonBRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(23, 41, 45), label = Sig), size = 4, position = position_dodge(0.9)) +
  ggtitle("Influence of Plant Diversity \non Bee Species Richness") +
  theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonBR

#-------------------------------------------------------------------#
#      Plant Diversity ~ Solitary Ground Nesting Bee Abundance      #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
SGNA <- read.csv("Bee_BarGraph_Solitary_Abundance.csv")

#PlantDiversity column in "SGNA" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
SGNA$PlantDiversity <- factor(SGNA$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonSGNAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Solitary Ground Nesting Bee Abundance
p1<-ggplot(data=SGNA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonSGNAlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Solitary Ground Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(96,100,185),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Solitary Ground Nesting Bee Abundance
PDonSGNA <- ggplot(data = SGNA, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonSGNAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(96, 100, 184), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nSolitary Ground Nesting Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonSGNA

#-------------------------------------------------------------------#
#       Plant Diversity ~ Social Ground Nesting Bee Abundance       #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
ScGNA <- read.csv("Bee_BarGraph_Social_Abundance.csv")

#PlantDiversity column in "ScGNA" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
ScGNA$PlantDiversity <- factor(ScGNA$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonScGNAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Social Ground Nesting Bee Abundance
p2<-ggplot(data=ScGNA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonScGNAlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Social Ground Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(92,159,248),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Social Ground Nesting Bee Abundance
PDonScGNA <- ggplot(data = ScGNA, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonScGNAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(91, 157, 248), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nSocial Ground Nesting Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonScGNA

#-------------------------------------------------------------------#
#                Plant Diversity ~ Bumblebee Abundance              #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
BBA <- read.csv("Bee_BarGraph_Bumblebee_Abundance.csv")

#PlantDiversity column in "BBA" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
BBA$PlantDiversity <- factor(BBA$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonBBAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Bumblebee Abundance
p3<-ggplot(data=BBA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonBBAlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Bumble Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(5.8,10.5,5.9),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Bumblebee Abundance
PDonBBA <- ggplot(data = BBA, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonBBAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(5.6, 10.4, 5.7), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nBumblebee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonBBA

#-------------------------------------------------------------------#
#          Plant Diversity ~ Cavity Nesting Bee Abundance           #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
CNA <- read.csv("Bee_BarGraph_CavityNester_Abundance.csv")

#PlantDiversity column in "CNA" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
CNA$PlantDiversity <- factor(CNA$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonCNAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Cavity Nesting Bee Abundance
p4<-ggplot(data=CNA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonCNAlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Cavity Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(5,23,42),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Cavity Nesting Bee Abundance
PDonCNA <- ggplot(data = CNA, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonCNAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(4.7, 21, 40.5), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nCavity Nesting Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonCNA

#-------------------------------------------------------------------#
#            Plant Diversity ~ Cleptoparasite Abundance             #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
CPA <- read.csv("Bee_BarGraph_Cleptoparasites_Abundance.csv")

#PlantDiversity column in "CPA" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
CPA$PlantDiversity <- factor(CPA$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonCPAlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Cleptoparasite Abundance
p5<-ggplot(data=CPA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Total Abundance")+
  geom_errorbar(PDonCPAlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Cleptoparasitic Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(1.3,4.9,6.9),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Cleptoparasite Abundance
PDonCPA <- ggplot(data = CPA, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonCPAlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(1.1, 4.6, 6.6), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nCleptoparasite Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonCPA

#Plot all of Amy's graphs in one figure
grid.arrange(p1, p2, p3, p4, p5, ncol=2)

#Plot all of Morgan's graphs in one figure
grid.arrange(PDonSGNA, PDonScGNA, PDonBBA, PDonCNA, PDonCPA, ncol = 2)

#-------------------------------------------------------------------#
#      Plant Diversity ~ Solitary Ground Nesting Bee Richness       #
#-------------------------------------------------------------------# 

#Clear environment
rm(list=ls())

#Read in data
SGNR <- read.csv("Bee_BarGraph_Solitary_Richness.csv")

#PlantDiversity column in "SGNR" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
SGNR$PlantDiversity <- factor(SGNR$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonSGNRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Solitary Ground Nesting Bee Richness
p1<-ggplot(data=SGNR,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Species Richness")+
  geom_errorbar(PDonSGNRlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Solitary Ground Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(12.7,19.8,22.4),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Solitary Ground Nesting Bee Richness
PDonSGNR <- ggplot(data = SGNR, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Species Richness") +
  geom_errorbar(PDonSGNRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(12.7, 19.8, 22.4), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nSolitary Ground Nesting Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonSGNR

#-------------------------------------------------------------------#
#        Plant Diversity ~ Social Ground Nesting Bee Richness       #
#-------------------------------------------------------------------# 

#Clear environment
rm(list=ls())

#Read in data
ScGNR <- read.csv("Bee_BarGraph_Social_Richness.csv")

#PlantDiversity column in "ScGNR" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
ScGNR$PlantDiversity <- factor(ScGNR$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonScGNRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Social Ground Nesting Bee Richness
p2<-ggplot(data=ScGNR,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Species Richness")+
  geom_errorbar(PDonScGNRlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Social Ground Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(5.3,6.8,6.9),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Social Ground Nesting Bee Richness
PDonScGNR <- ggplot(data = ScGNR, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Species Richness") +
  geom_errorbar(PDonScGNRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(5.3, 6.8, 6.8), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nSocial Ground Nesting Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonScGNR

#-------------------------------------------------------------------#
#                Plant Diversity ~ Bumblebee Richness               #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
BBR <- read.csv("Bee_BarGraph_Bumblebee_Richness.csv")

#PlantDiversity column in "BBR" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
BBR$PlantDiversity <- factor(BBR$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonBBRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Bumblebee Richness
p3<-ggplot(data=BBR,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Species Richness")+
  geom_errorbar(PDonBBRlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Bumble Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(3.7,3.9,3.4),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Bumblebee Richness
PDonBBR <- ggplot(data = BBR, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Species Richness") +
  geom_errorbar(PDonBBRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(3.6, 3.9, 3.3), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nBumblebee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonBBR

#-------------------------------------------------------------------#
#          Plant Diversity ~ Cavity Nesting Bee Richness            #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
CNR <- read.csv("Bee_BarGraph_CavityNester_Richness.csv")

#PlantDiversity column in "CNR" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
CNR$PlantDiversity <- factor(CNR$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonCNRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Cavity Nesting Bee Richness
p4<-ggplot(data=CNR,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Species Richness")+
  geom_errorbar(PDonCNRlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Cavity Nesting Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(2.4,7.1,8.2),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Cavity Nesting Bee Richness
PDonCNR <- ggplot(data = CNR, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Species Richness") +
  geom_errorbar(PDonCNRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(2.4, 7.1, 8.2), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nCavity Nesting Bee Species Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonCNR

#-------------------------------------------------------------------#
#            Plant Diversity ~ Cleptoparasite Richness              #
#-------------------------------------------------------------------#

#####Don't have the necessary file!

#Clear environment
rm(list=ls())

#Read in data
CPR <- read.csv("Bee_BarGraph_Cleptoparasites_Richness.csv")

#PlantDiversity column in "CPR" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
CPR$PlantDiversity <- factor(CPR$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
PDonCPRlimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Plant Diversity vs. Cleptoparasite Richness
p5<-ggplot(data=CPA,aes(x=factor(PlantDiversity),y=Measure))+
  geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Species Richness")+
  geom_errorbar(PDonCPRlimits,position=position_dodge(0.9),width=0.25)+
  ggtitle("Cleptoparasitic Bees")+
  geom_text(aes(x=factor(PlantDiversity),y=c(1.2,3.8,5.4),label=Sig),size=4,position=position_dodge(0.9))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#Morgan's plot: Plant Diversity vs. Cleptoparasite Richness
PDonCPR <- ggplot(data = CPR, aes(x = PlantDiversity, y = Measure)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Bee Abundance") +
  geom_errorbar(PDonCPRlimits, position = position_dodge(0.9), width = 0.25) +
  geom_text(aes(x = PlantDiversity, y = c(1.2, 3.8, 5.4), label = Sig), size = 4, position = position_dodge(0.9)) +
  theme_bw() +
  ggtitle("Influence of Plant Diversity on \nCleptoparasite Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
PDonCPR

#Plot all of Amy's graphs in one figure
grid.arrange(p1, p2, p3, p4, ncol=2)

#Plot all of Morgan's graphs in one figure
grid.arrange(PDonSGNR, PDonScGNR, PDonBBR, PDonCNR, ncol = 2)
#Include PDonCPR upon receiving datafile

#-------------------------------------------------------------------#
#                     Chao1 Richness by Site                        #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
SiteChao <- read.csv("Bee_BarGraph_Site_Chao.csv")

#####How did you calculate Chao1 richness estimate?
#####Does this value include both years? What do the graphs look like if years one and two are separated?

#PlantDiversity column in "SiteChao" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
SiteChao$PlantDiversity <- factor(SiteChao$PlantDiversity, c("Low", "Medium", "High"))

#Site column in "SiteChao" dataframe is an incorrect order; reorder as follows.
SiteChao$Site <- factor(SiteChao$Site,c("Bo","Mc","Sh","El","Ka","Pl","Sl","Cr","NS","Gr/Pe"))

#Create values for error bars
SiteChaolimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Chao1 Richness by Site
p<-ggplot(data=SiteChao,aes(x=factor(Site),y=Measure, fill=PlantDiversity))
p+geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Site",y="Chao1 Richness Estimate")+
  geom_errorbar(SiteChaolimits,position=position_dodge(0.9),width=0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Morgan's plot: Chao1 Richness by Site
SiteChaoGraph <- ggplot(data = SiteChao, aes(x = Site, y = Measure, fill = PlantDiversity)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Site", y = "Chao1 Richness Estimate") +
  geom_errorbar(SiteChaolimits, position = position_dodge(0.9), width=0.25) +
  theme_bw() +
  ggtitle("Chao1 Richness Estimates by Site") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
SiteChaoGraph

#Chao1 richness Treatment
bm <- read.csv("Bee_BarGraph_Treatment_Chao.csv")
names(bm)
bm$PlantDiversity <- factor(bm$PlantDiversity, c("Low", "Medium", "High"))

limits<-aes(ymax=Measure+SE, ymin=Measure-SE)

p<-ggplot(data=bm,aes(x=factor(PlantDiversity),y=Measure))
p+geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Chao1 Richness Estimate")+
  geom_errorbar(limits,position=position_dodge(0.9),width=0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


#-------------------------------------------------------------------#
#                  Chao1 Richness by Treatment                      #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
TreatChao <- read.csv("Bee_BarGraph_Treatment_Chao.csv")

#PlantDiversity column in "TreatChao" dataframe is brought in as High, Low, Medium; change order to Low, Medium, High.
TreatChao$PlantDiversity <- factor(TreatChao$PlantDiversity, c("Low", "Medium", "High"))

#Create values for error bars
TreatChaolimits <- aes(ymax = Measure + SE, ymin = Measure - SE)

#Amy's plot: Chao1 Richness by Site
p<-ggplot(data=TreatChao,aes(x=factor(PlantDiversity),y=Measure))
p+geom_bar(stat="identity",position=position_dodge(0.9))+
  labs(x="Plant Diversity",y="Chao1 Richness Estimate")+
  geom_errorbar(TreatChaolimits,position=position_dodge(0.9),width=0.25)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

#Morgan's plot: Chao1 Richness by Site
TreatChaoGraph <- ggplot(data = TreatChao, aes(x = PlantDiversity, y = Measure, fill = PlantDiversity)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  labs(x = "Plant Diversity", y = "Chao1 Richness Estimate") +
  geom_errorbar(TreatChaolimits, position = position_dodge(0.9), width=0.25) +
  theme_bw() +
  ggtitle("Chao1 Richness Estimates by Treatment") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
TreatChaoGraph