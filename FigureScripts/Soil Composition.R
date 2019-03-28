#-------------------------------------------------------------------#
#                          SOIL COMPOSITION                         #
#-------------------------------------------------------------------#

#Research Question:  How does soil composition within nesting plots between sites?

#Objective:  
#Visualize soil composition via ternary graphs (a la Cane 1991) of each site and all of the sites together

#Start ####

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/mmackert/Data")

#Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtern)
library(ggpubr)

#Read in data
soils <- read.csv("soil/Reduced Analysis Results.csv")

#Rename columns in "Reduced Analysis Results"
colnames(soils) <- c("Sample ID", "Site", "Depth", "Sand", "Silt", "Clay", "Texture")

#Plunkett ####
#-------------------------------------------------------------------#
#                             Plunkett                              #
#-------------------------------------------------------------------#
#Create Plunkett dataframe
PLsoils <- soils[49:56, ]

#Create Plunkett ternary diagram 
PLsoilstern <- ggtern(data = PLsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#FFFF33") +
  ggtitle("Plunkett Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
PLsoilstern

#Bowman ####
#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#
#Create Bowman dataframe
BOsoils <- soils[33:40, ]

#Create Bowman ternary diagram 
BOsoilstern <- ggtern(data = BOsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#E41A1C") +
  ggtitle("Bowman Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
BOsoilstern

#Kaldenberg ####
#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#
#Create Kaldenberg dataframe
KAsoils <- soils[1:8, ]

#Create Kaldenberg ternary diagram 
KAsoilstern <- ggtern(data = KAsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#4DAF4A") +
  ggtitle("Kaldenberg Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
KAsoilstern

#McClellan ####
#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#
#Create McClellan dataframe
MCsoils <- soils[17:24, ]

#Create McClellan ternary diagram 
MCsoilstern <- ggtern(data = MCsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#984EA3") +
  ggtitle("McClellan Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
MCsoilstern

#Sloan ####
#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#
#Create Sloan dataframe
SLsoils <- soils[57:64, ]

#Create Sloan ternary diagram 
SLsoilstern <- ggtern(data = SLsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#F781BF") +
  ggtitle("Sloan Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
SLsoilstern

#Sheller ####
#-------------------------------------------------------------------#
#                              Sheller                              #
#-------------------------------------------------------------------#
#Create Sheller dataframe
SHsoils <- soils[41:48, ]

#Create Sheller ternary diagram 
SHsoilstern <- ggtern(data = SHsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21, size = 3, fill = "#A65628") +
  ggtitle("Sheller Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
SHsoilstern

#Cretsinger ####
#-------------------------------------------------------------------#
#                           Cretsinger                              #
#-------------------------------------------------------------------#
#Create Cretsinger dataframe
CRsoils <- soils[9:16, ]

#Create Cretsinger ternary diagram 
CRsoilstern <- ggtern(data = CRsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#377EB8") +
  ggtitle("Cretsinger Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15))
CRsoilstern

#Peckumn ####
#-------------------------------------------------------------------#
#                              Peckumn                              #
#-------------------------------------------------------------------#
#Create Peckumn dataframe
PEsoils <- soils[25:32, ]

#Create Peckumn ternary diagram 
PEsoilstern <- ggtern(data = PEsoils,
                      aes(Sand, Silt, Clay)) +
  geom_point(shape = 21,
             size = 3,
             fill = "#FF7F00") +
  ggtitle("Peckumn Soil Composition") +
  theme_bw() +
  theme_showarrows() +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 15)) +
  theme_showarrows()
PEsoilstern

#All Sites ####
#-------------------------------------------------------------------#
#                           Full Ternary                            #
#-------------------------------------------------------------------#
#Set "sitecolors" to be "Set1" from color brewer; makes formatting the legend easier
sitecolors <- RColorBrewer::brewer.pal(8, "Set1")

#Replace site names with site numbers
soils <- soils %>%
  mutate(SiteNo. = case_when(
    Site == "Bowman" ~ "1",
    Site == "Cretsinger" ~ "2",
    Site == "Kaldenberg" ~ "5",
    Site == "McClellan" ~ "6",
    Site == "Peckumn" ~ "8",
    Site == "Plunkett" ~ "9",
    Site == "Sheller" ~ "10",
    Site == "Sloan" ~ "11"
  ))

#For the legend in the ternary plot to be ordered numerically, change the site numbers to factor
soils$SiteNo. <- as.factor(soils$SiteNo.)

#Create full ternary diagram colored by site
fullsoilstern <- ggtern(data = soils,
                        aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(aes(color = SiteNo.),
                 size = 3) +
  geom_point(shape = 21,
             size = 3,
             color = "black") +
  #ggtitle("Soil Composition within Nesting Plots \nof All Sites") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  guides(color = guide_legend(override.aes = list(shape = 21,
                                                  fill = sitecolors,
                                                  color = "black"))) +
  scale_color_manual(values = sitecolors,
                     name = "Site Number")
fullsoilstern

#-------------------------------------------------------------------#
#                        Full Stacked Bar Graph                     #
#-------------------------------------------------------------------#
#Read in data
soilsbar <- read.csv("soil/Reduced Analysis Results Formatted for Bar Graph.csv")

#Rename columns in "soilsbar"
colnames(soilsbar) <- c("Sample ID", "Site", "Soil", "Percentage")

#Figure out proportions of each soil type for each site
#Determine sum of soil types for each site (also helps double check data entry!)
sumsoils <- soilsbar %>%
  group_by(Site) %>%
  summarise(TotalPercentage = sum(Percentage))

#Join these two datasets together by site
soilsbarjoined <- full_join(soilsbar, sumsoils, by = c("Site")) 

#Determine proportions using new summed values
soilsbarjoined$Proportion <- (soilsbarjoined$Percentage/soilsbarjoined$TotalPercentage)*100

#Use "CPCOLS" for colors if using "Plot Colour Helper" add-in; colors listed in "CPCOLS" below are random
scale_fill_manual(values = CPCOLS)
CPCOLS <- c("#1F78B4", "#FF8C00", "#AD3CB5")

#This color scheme works well for colorblindness and is printer friendly!
soilcolors <- c("#1B9E77", "#D95F02", "#7570B3")

#Once colors are determined, include them here:
#No black outline on this graph because IT WON'T DO IT RIGHT.
fullsoilsbar <- ggplot(soilsbarjoined,
                       aes(x = Site, y = Proportion, fill = Soil)) +
  geom_bar(stat = "identity") +
  ggtitle("Soil Composition within Nesting Plots \nat All Sites") +
  theme_bw() +
  scale_fill_manual(values = soilcolors) +
  theme(plot.title = element_text(size = 15,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(legend.title.align = 0.5) +
  labs(y = "Composition (%)",
       fill = "Soil Type")
fullsoilsbar

#-------------------------------------------------------------------#
#                        Full Ternary with RRW                      #
#-------------------------------------------------------------------#
#Apply county names to corresponding sites (ugly but it works)
soils <- soils %>%
  mutate(County = ifelse(Site == "Plunkett", "Story",
                         ifelse(Site == "Bowman", "Dallas",
                                ifelse(Site == "Kaldenberg", "Jasper",
                                       ifelse(Site == "McClellan", "Jasper",
                                              ifelse(Site == "Sloan", "Buchanan",
                                                     ifelse(Site == "Sheller", "Grundy",
                                                            ifelse(Site == "Cretsinger", "Guthrie",
                                                                   ifelse(Site == "Peckumn", "Greene",
                                                                          ifelse(Site == "Greving", "Carroll",
                                                                                 ifelse(Site == "NealSmith", "Jasper",
                                                                                        ifelse(Site == "Elkader", "Clayton",
                                                                                               NA
                                                                                        ))))))))))))

#Check to make sure mutate function worked
soils %>%
  group_by(Site, County) %>%
  summarise()

#Apply RRW (yes or no) to counties
soils <- soils %>%
  mutate(RRW = ifelse(County == "Story", "Non-RRW",
                      ifelse(County == "Dallas", "RRW",
                             ifelse(County == "Jasper", "Non-RRW",
                                    ifelse(County == "Buchanan", "Non-RRW",
                                           ifelse(County == "Grundy", "Non-RRW",
                                                  ifelse(County == "Guthrie", "RRW",
                                                         ifelse(County == "Greene", "RRW",
                                                                ifelse(County == "Carroll", "RRW",
                                                                       ifelse(County == "Clayton", "Non-RRW",
                                                                              NA
                                                                       ))))))))))
#Check to make sure RRW worked
soils %>%
  group_by(Site, RRW) %>%
  summarise()
#Good!

#Create full ternary diagram colored by site
fullsoilsternRRW <- ggtern(data = soils,
                        aes(x = Sand, y = Silt, z = Clay)) +
  geom_mask() +
  geom_point(aes(color = RRW),
             size = 4) +
  geom_point(shape = 21,
             size = 4) +
  theme_bw() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 22,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15)) +
  theme_nomask()
fullsoilsternRRW

#-------------------------------------------------------------------#
#                     Soil Composition ANOVA                        #
#-------------------------------------------------------------------#
#Reformat from wide to long format
soils.long <- gather(soils, key = "soil.type", value = "percentage", Sand, Silt, Clay)

#Exploratory boxplot
ggboxplot(soils.long,
          x = "soil.type",
          y = "percentage",
          color = "soil.type",
          palette = "uchicago",
          ylab = "Percentage",
          xlab = "Soil Type") +
  stat_compare_means(method = "anova",
                     label.y = 90) +
  stat_compare_means(label = "p.signif",
                     method = "t.test",
                     ref.group = ".all.") +
  rremove("legend")

#Exploraty mean line graph
ggline(soils.long, x = "soil.type",
       y = "percentage",
       add = c("mean_se", "jitter"),
       ylab = "Composition (%)",
       xlab = "Soil Type") +
  stat_compare_means(method = "anova",
                     label.y = 90) +
  stat_compare_means(label = "p.signif",
                     method = "t.test",
                     ref.group = ".all.")

#ANOVA of percent composition of each soil type
soil.aov <- aov(percentage ~ soil.type,
                data = soils.long)
summary(soil.aov)

#Plot results of ANOVA and we see that homogeneity of variance assumption is violated
plot(soil.aov, 1)

#ANOVA with no assumption of equal variances
oneway.test(percentage ~ soil.type,
            data = soils.long)

#QQ plot shows residuals fall mostly on the line, we good
plot(soil.aov, 2)

#Data dictionary ####
#Sample ID = the identification number and letter combination given to each sample before analysis
#Site = Site name
#Beginning Depth = the shallowest depth of each sample
#Ending Depth = the deepest depth of each sample
#Sand/Silt/Clay = Percentage of each soil type in the sample
#Texture = soil classification for each sample
#Old code ####
fullsoilsternblack <- ggtern(data = soils,
                             aes(x = Sand, y = Silt, z = Clay)) +
  geom_point(shape = 20,
             size = 3.5,
             color = "black",
             fill = "black") +
  #ggtitle("Soil Composition within Nesting Plots \nof All Sites") +
  theme_bw() +
  theme_showarrows() +
  theme(legend.title = element_text(size = 16),
        legend.title.align = 0.5,
        legend.text = element_text(size = 12),
        legend.position = c(0.85, 0.80)) +
  theme(plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5)) +
  theme(text = element_text(size = 15))
fullsoilsternblack
