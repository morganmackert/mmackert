#------------------------------------------------#
#                     MODELS                     #
#------------------------------------------------#

#Clear environment
rm(list=ls())

#Set working directory
setwd("~/ISU/Project/Data")

#Read in data
beeMorgan <- read.csv("Mackert Reduced data set.csv")

#Load libraries
library(lme4)
require(phia)
library(car)
library(AER)
library(ggplot2)

#Check for N/As
any(is.na(beeMorgan))

#Model
#####FARNAZ: What is this model specifying?
BE <- glm(Total_Bees~ ., data = beeMorgan, family = poisson)

#Test for overdispersion
dispersiontest(BE, trafo = 1)
#No overdispersion; we set alpha = 1, if resulting alpha is > 1, we have over dispersion. Our result was 0.76.


#No Over dispersion so use glm for Total_Bees
#####FARNAZ: Do we need to include the interaction term for Site and Month?
mm <- glm(Total_Bees ~ Site + Month, family = poisson, data = beeMorgan)
summary(mm)

#####FARNAZ: What is this plot showing us again? I forget.
plot(interactionMeans(mm), cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1)

##############
#Test glmer for Total_Bees
mmm <- glmer(Total_Bees ~ Site + Month + (1|Site:Month), family = poisson, data = beeMorgan)
summary(mmm)

#####FARNAZ: What is the purpose of these functions? What is allEffects?
plot(allEffects(mmm))
allEffects(mmm)

#Plot interaction means.
plot(interactionMeans(mmm), cex = 2, cex.main = 2, cex.lab = 2, cex.axis = 1)

#####################
#Use lm for plant coverage data
plant <- lm(Total_Plants ~ Site + Month, data = beeMorgan)
summary(plant)

plot(interactionMeans(plant),cex=2,cex.main=2, cex.lab=2,cex.axis=1)

#############################
#Correlation test between plant cover and bee abundance
#####FARNAZ: So this tells us that blooming forb coverage is positively correlated with bee abundance. Is that right?
cor.test(beeMorgan$Total_Plants, beeMorgan$Total_Bees)

#Correlation plot in ggplot for Forb and bee abundance
ggplot(beeMorgan, aes(Total_Plants, Total_Bees)) +
  geom_point(aes(x = Total_Plants, y = Total_Bees, color = Site),size = 2) +
  geom_smooth(method = "lm") +
  xlab("Forb Cover (%)") + #####FARNAZ: I still don't understand how we can have 600% coverage. Help. :(
  ylab("Bee Abundance") +
  #coord_cartesian(ylim = c(2500, 12500))+ 
  #facet_wrap(~ Trt)+
  theme_bw() +
  theme(text = element_text(size=28), legend.text = element_text(size = 18), axis.ticks = element_blank(), # remove axis ticks
        axis.title.x = element_text(size=28), # remove x-axis labels
        axis.title.y = element_text(size=28), # remove y-axis labelspanel.background = element_blank(),
        panel.grid.major = element_blank(), #remove major-grid labels
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="black", colour="black"), #remove minor-grid labels
        strip.text.x=element_text(color="white"),
        plot.background = element_blank())