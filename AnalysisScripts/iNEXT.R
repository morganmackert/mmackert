#####################################################################
#                          INEXT PRACTICE                           #
#####################################################################

#Load libraries
library(ggplot2)
library(iNEXT)

#Spider example from Hsieh et al 2016 paper
data(spider)
iNEXT(spider, q = c(0, 1, 2), datatype = "abundance")
ggiNEXT(spider, type = 1, se = TRUE, grey = FALSE)

#Bird example from Hsieh et al 2016 paper including graphics
data(bird)
str(bird)
out <- iNEXT(bird, q = c(0, 1, 2), datatype = "abundance")

ggiNEXT(out, type = 1, facet.var = "site")
ggiNEXT(out, type = 1, facet.var = "site", grey = TRUE)
ggiNEXT(out, type = 1, facet.var = "order")

#Trying it out with a subset of the previous data!
#Set working directory and read in the data
setwd("~/ISU/Project/mmackert/Data")
PlunkettBowman <- read.csv("bees/working/PlunkettBowman.csv")
#Data must be as a dataframe
PlunkettBowman <- as.data.frame(PlunkettBowman)
#Change the rownames to the species rather than numbers
PlunkettBowman2 <- PlunkettBowman[,-1]
rownames(PlunkettBowman2) <- PlunkettBowman[,1]
#Check the dataframe to make sure all is well
str(PlunkettBowman2)

#RUN THE TEST
out4 <- iNEXT(PlunkettBowman2, q = c(0, 1, 2), datatype = "abundance")

#Make pretty graphs
ggiNEXT(out4, type = 1, facet.var = "site")
ggiNEXT(out4, type = 1, facet.var = "order")

#Now try it with the whole previous dataset!
#Read in the data
iNEXTAbundance <- read.csv("bees/working/iNEXTAbundance.csv")
#Data must be as a dataframe if not brought in as dataframe.
iNEXTAbundance <- as.data.frame(iNEXTAbundance)
#Change the rownames to the species rather than numbers
iNEXTAbundance2 <- iNEXTAbundance[,-1]
rownames(iNEXTAbundance2) <- iNEXTAbundance[,1]
#Check the dataframe to make sure all is well
str(iNEXTAbundance2)

#RUN THE TEST
iNEXTResult <- iNEXT(iNEXTAbundance2, q = c(0, 1, 2), datatype = "abundance")

#Make pretty graphs
ggiNEXT(iNEXTResult, type = 1, facet.var = "site")
