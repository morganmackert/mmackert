#####################################################################
#                           REGRESSIONS                             #
#####################################################################

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Previous Data/Data Files")

#Load libraries
library(ggplot2)

#-------------------------------------------------------------------#
#                Percent Bare Ground ~ Bee Abundance                #
#-------------------------------------------------------------------#
#Read in the data
et <- read.csv("ETrap_Bees.csv")
#####How is "BareGround" calculated? Is it the average of all quadrats?
#####Yes

#Year column in "et" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list <- as.numeric(et$Year)
pch.list

#Year column in "et" dataframe is brought in as an integer. Change to factor for Morgan's plot.
et$Year <- as.factor(et$Year)

#Amy's plot: Percent Bare Ground vs. Bee Abundance
plot(et$BareGround, et$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance",
     pch = c(pch.list), col = 'black')
model=lm(et$Ind~et$BareGround)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))
model2=lm(et$Ind~0+et$BareGround)
summary(model2)
abline(model2, lty="dotted")
#####Is the second model forcing the intercept through zero? How do you know you need to have this?
#####Yes, force the intercept through zero. Looks at 0% bare ground and 0 bees.

#Model for bee abundance predicted by bare ground
BGonBA <- lm(Ind ~ BareGround, data = et)
summary(BGonBA)

#Find intercept and slope to plot best fit line on graph
coef(BGonBA)

#Morgan's plot: Percent Bare Ground vs. Bee Abundance plot using ggplot2
BGonBAplot <- ggplot(et, aes(x = BareGround, y = Ind)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = -0.7025809, slope = 0.3164081) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground \non Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BGonBAplot

#-------------------------------------------------------------------#
#           Blooming Forb and Weed Species ~ Bee Abundance          #
#-------------------------------------------------------------------#
#Clear environment
rm(list=ls())

#Read in data
nq <- read.csv("New_Bees_Format.csv")
#####Floral index in this data set DOES include weed species as well as forbs.
#####PercentCover in "New_Bees_Format.csv" is the average floral coverage for all 40 quadrats in year 1; all 50 in year 2.
#####For example: McClellan Year 1 (Site 9) had 0.3% coverage in all ten quadrats during the first sample of the year and nothing beyond that, so the average over all 4 samples is 0.3/4=0.075.
#####Quadrats in "New_Bees_Format.csv" is the proportion of quadrats over the entire year that contained blooming species.
#####For example: McClellan Year 1 (Site 9) had 1 quadrat during the entire year with anything blooming, so the proportion would be 1/40=0.025.

#Year column in "nq" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list<-as.numeric(nq$Year)
pch.list

#Year column in "nq" dataframe is brought in as an integer. Change to factor for Morgan's plot.
nq$Year <- as.factor(nq$Year)

#Amy's plot: Number Quadrats vs. Bee Abundance
plot(nq$Quadrats,nq$TotalAbundance,
     xlab="Frequency of Blooming Species",ylab="Bee Abundance",
     pch=c(pch.list),col='black')
model=lm(nq$TotalAbundance~nq$Quadrats)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by frequency of blooming species
BSonBA <- lm(TotalAbundance ~ Quadrats, data = nq)
summary(BSonBA)

#Find intercept and slope to plot best fit line on graph
coef(BSonBA)

#Morgan's plot: Number of blooming forb/weed species vs. Bee Abundance
#####What does "Quadrats" column depict exactly? Total coverage? Average coverage?
#####Average coverage!
#####What is "Frequency of Blooming Species"?
#####Frequency = Percent Coverage
BSonBAplot <- ggplot(nq, aes(x = Quadrats, y = TotalAbundance)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 123.5586, slope = 297.5132) +
  theme_bw() +
  labs(x = "Frequency of Blooming Species", y = "Bee Abundance") +
  ggtitle("Influence of Blooming Forb and Weed Species on Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
BSonBAplot

#-------------------------------------------------------------------#
#             Quadrats and Percent Forest ~ Bee Abundance           #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
bee <- read.csv("New_Bees_Format.csv")

#Load libraries
library(MASS)
library(scatterplot3d)

#Model for bee abundance predicted by frequency of blooming species and percent forest
PFQonBA <- lm(TotalAbundance ~ Quadrats + PercentForest, data = bee)
summary(PFQonBA)

#Choose an appropriate model by AIC using "stepAIC" function
step <- stepAIC(PFQonBA, direction = "both")
#####How to interpret output of this function?
#####Does this mean Percent Forest has a negligible effect on bee abundance?

#Perform ANOVA on stepAIC output
step$anova

#Amy's plot: Create a 3D view of frequency of blooming species vs. percent forest coverage vs. bee abundance
plot <- scatterplot3d(bee$Quadrats,rank(bee$PercentForest), bee$TotalAbundance, xlab="Frequency of Blooming Species", ylab="Ranked Percent Forest", zlab="Total Bee Abundance")
plot$plane3d(lm(bee$TotalAbundance~bee$Quadrats+rank(bee$PercentForest))) #adds a plane to the graph
plot<-scatterplot3d(bee$Quadrats,rank(bee$PercentForest),bee$TotalAbundance,angle=20) #will put data at an angle without adding plane
#####Why is percent forest ranked?

#Morgan's plot: Create a 3D view of frequency of blooming species vs. percent forest coverage vs. bee abundance
PFQonBAplot <- with(bee, {
  scatterplot3d(Quadrats, PercentForest, TotalAbundance,
                color = "red", pch = 19,
                type = "h",
                main = "Influence of Blooming Species Frequency and \nPercent Forest Coverage on Bee Abundance",
                xlab = "Frequency of Blooming Species",
                ylab = "Percent Forest Coverage",
                zlab = "Bee Abundance")
})
PFQonBAplot

#Add in a plane to 3D scatterplot
PFQonBAplot$plane3d(lm(bee$TotalAbundance ~ bee$Quadrats + bee$PercentForest))

#-------------------------------------------------------------------#
#               Percent Forest ~ Cavity Nester Abundance            #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
pf <- read.csv("Guild_Year_Abundance.csv")

#Year column in "pf" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list <- as.numeric(pf$Year)
pch.list

#Year column in "pf" dataframe is brought in as an integer. Change to factor for Morgan's plot.
pf$Year <- as.factor(pf$Year)

#Amy's plot: Percent Forest Coverage vs. Cavity Nesting Bee Abundance
plot(pf$PercentForest, pf$CavityNesters,
     xlab="Percent Forested Area in 1km Radius",ylab="Cavity Nesting Bee Abundance",
     pch=c(pch.list),col='black')
model = lm(pf$CavityNesters ~ pf$PercentForest)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by forest coverage
PFonCNA <- lm(CavityNesters ~ PercentForest, data = pf)
summary(PFonCNA)

#Find intercept and slope to plot best fit line on graph
coef(PFonCNA)

#Morgan's plot: Percent Forest Coverage vs. Cavity Nesting Bee Abundance plot using ggplot2
PFonCNAplot <- ggplot(pf, aes(x = PercentForest, y = CavityNesters)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 12.8147487, slope = 0.2660526) +
  theme_bw() +
  labs(x = "Percent Forested Area in 1km Radius", y = "Cavity Nesting Bee Abundance") +
  ggtitle("Influence of Forest Coverage on \nCavity Nesting Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
PFonCNAplot

#-------------------------------------------------------------------#
#              Percent Forest ~ Cavity Nester Richness              #
#-------------------------------------------------------------------#

#Clear environment
rm(list=ls())

#Read in data
cn <- read.csv("Guild_Year_Richness.csv")

#Year column in "cn" dataframe is brought in as an integer. Change to numeric for Amy's plot.
pch.list <- as.numeric(cn$Year)
pch.list

#Year column in "cn" dataframe is brought in as an integer. Change to factor for Morgan's plot.
cn$Year <- as.factor(cn$Year)

#Amy's plot: Percent Forest Coverage vs. Cavity Nesting Bee Richness
plot(cn$PercentForest, cn$CavityNesters,
     xlab = "Percent Forested Area in a 1km Radius", ylab = "Cavity Nesting Species Richness",
     pch = c(pch.list), col = 'black')
model = lm(cn$CavityNesters ~ cn$PercentForest)
model
summary(model)
abline(model)
legend("topleft",bty="n",
       legend=paste("R2 is",format(summary(model)$adj.r.squared,digits=4)))

#Model for bee abundance predicted by forest coverage
PFonCNR <- lm(CavityNesters ~ PercentForest, data = cn)
summary(PFonCNR)

#Find intercept and slope to plot best fit line on graph
coef(PFonCNR)

#Morgan's plot: Percent Forest Coverage vs. Cavity Nesting Bee Richness
PFonCNRplot <- ggplot(cn, aes(x = PercentForest, y = CavityNesters)) +
  geom_point(aes(shape = Year, color = Year), size = 3) +
  geom_abline(intercept = 3.62212627, slope = 0.09367731) +
  theme_bw() +
  labs(x = "Percent Forested Area in 1km Radius", y = "Cavity Nesting Bee Richness") +
  ggtitle("Influence of Forest Coverage on \nCavity Nesting Bee Richness") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5)) +
  theme(legend.text = element_text(size = 10))
PFonCNRplot

#Scatterplot of Percent Forest Coverage vs. Cavity Nesting Bee Richness
cor.test(cn$PercentForest, cn$CavityNesters, method = "spearman")
#####Throws warning: Cannot compute exact p-value with ties
#####How to interpret these results?
#####Interpret p-value regardless of warning; 0.1797 >> 0.05.

#####Why do we need to plot the rank?
#####Plot rank because Elkader pulled data in weird ways.
plot(rank(cn$PercentForest), rank(cn$CavityNesters),
     xlab="Percent Forested Area in 1km Radius Rank", ylab="Cavity Nesting Specied Richness Rank",
     pch=c(pch.list),col='black')
