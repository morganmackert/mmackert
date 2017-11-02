#-------------------------------------------------------------------#
#                  Chao Richness Estimators by Site                 #
#                             Years 1-4                             #
#-------------------------------------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Previous Data/Data Files")

#Load libraries
library(fossil)

#####Function "spp.est" returns a table: N.obs = Total sample size, S.obs = Number of observed species, Chao1 = Chao Species Estimation, ACE = Abundance-based Coverage Estimator, Jack1 = First Order Jacknife Estimator.

#####What is Chao1??
#"Chao1 estimates total species richness as SChao = Sobs + (n1^2/(2*n2)), where Sobs is the number of observed species, n1 is the number of singletons (species captured once), and n2 is the number of doubletons (species captured twice). Chao noted that this index is particularly useful for data sets skewed toward the low-abundance classes."
#(Hughes, Jennifer B. et al. “Counting the Uncountable: Statistical Approaches to  Estimating Microbial Diversity.” Applied and Environmental Microbiology 67.10 (2001): 4399–4406. PMC. Web. 1 Aug. 2017.)

#-------------------------------------------------------------------#
#                              Plunkett                             #
#-------------------------------------------------------------------#

#Read in data
Plunkett <- read.csv("Plunketts.csv")
#####Do the numbers following the site names in the column headings correspond to sampling period?
#####Numbers following site names correspond to sampling period for BOTH years.

#Remove the first column (Site.Code) and change "Plunkett" from data.frame to a matrix
PlunkettMatrix <- as.matrix(Plunkett[,-1])

#Use "spp.est" function to estimate species diversity
PLspp.est1234 <- spp.est(PlunkettMatrix, abund = TRUE)
#####How is it possible to have 16.6 observed species?
#####Because this is the estimate.
#####Why do I get different results each time I run the function?
#####Because it's an estimate.

#Export output as .csv file
write.csv(PLspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/PLspp.est1234.csv")

#-------------------------------------------------------------------#
#                              Bowman                               #
#-------------------------------------------------------------------#

#Read in data
Bowman <- read.csv("Bowman.csv")

#Remove the first column (Site Code) and change "Bowman" from data.frame to a matrix
BowmanMatrix <- as.matrix(Bowman[,-1])

#Use "spp.est" function to estimate species diversity
BOspp.est1234 <- spp.est(BowmanMatrix, abund = TRUE)

#Export output as .csv file
write.csv(BOspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/BOspp.est1234.csv")

#-------------------------------------------------------------------#
#                           Kaldenberg                              #
#-------------------------------------------------------------------#

#Read in data
Kaldenberg <- read.csv("Kaldenberg.csv")

#Remove the first column (Site Code) and change "Bowman" from data.frame to a matrix
KaldenbergMatrix <- as.matrix(Kaldenberg[,-1])

#Use "spp.est" function to estimate species diversity
KAspp.est1234 <- spp.est(KaldenbergMatrix, abund = TRUE)

#Export output as .csv file
write.csv(KAspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/KAspp.est1234.csv")

#-------------------------------------------------------------------#
#                            McClellan                              #
#-------------------------------------------------------------------#

#Read in data
McClellan <- read.csv("McClellan.csv")

#Remove the first column (Site Code) and change "McClellan" from data.frame to a matrix
McClellanMatrix <- as.matrix(McClellan[,-1])

#Use "spp.est" function to estimate species diversity
MCspp.est1234 <- spp.est(McClellanMatrix)

#Export output as .csv file
write.csv(MCspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/MCspp.est1234.csv")

#-------------------------------------------------------------------#
#                               Sloan                               #
#-------------------------------------------------------------------#

#Read in data
Sloan <- read.csv("Sloan.csv")

#Remove the first column (Site Code) and change "Sloan" from data.frame to a matrix
SloanMatrix <- as.matrix(Sloan[,-1])

#Use "spp.est" function to estimate species diversity
SLspp.est1234 <- spp.est(SloanMatrix)

#Export output as .csv file
write.csv(SLspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/SLspp.est1234.csv")

#-------------------------------------------------------------------#
#                             Sheller                               #
#-------------------------------------------------------------------#

#Read in data
Sheller <- read.csv("Sheller.csv")

#Remove the first column (Site Code) and change "Sheller" from data.frame to a matrix
ShellerMatrix <- as.matrix(Sheller[,-1])

#Use "spp.est" function to estimate species diversity
SHspp.est1234 <- spp.est(ShellerMatrix)

#Export output as .csv file
write.csv(SHspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/SHspp.est1234.csv")

#-------------------------------------------------------------------#
#                            Cretsinger                             #
#-------------------------------------------------------------------#

#Read in data
Cretsinger <- read.csv("Cretsinger.csv")

#Remove the first column (Site Code) and change "Cretsinger" from data.frame to a matrix
CretsingerMatrix <- as.matrix(Cretsinger[,-1])

#Use "spp.est" function to estimate species diversity
CRspp.est1234 <- spp.est(CretsingerMatrix)

#Export output as .csv file
write.csv(CRspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/CRspp.est1234.csv")

#-------------------------------------------------------------------#
#                        Greving and Peckumn                        #
#-------------------------------------------------------------------#

#Read in data
GRandPE <- read.csv("Greving_Peckumn.csv")
#####Columns labeled as "Greving" but include both sites.

#Remove the first column (Site Code) and change "GandP" from data.frame to a matrix
GRandPEMatrix <- as.matrix(GRandPE[,-1])

#Use "spp.est" function to estimate species diversity
GRandPEspp.est1234 <- spp.est(GRandPEMatrix)

#Export output as .csv file
write.csv(GRandPEspp.est1234, file = "C:/Users/morga/Documents/ISU/Project/mmackert/Graphs/ChaoRichness/GRandPEspp.est1234.csv")