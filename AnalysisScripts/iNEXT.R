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

#Trying it out with previous data!
#Read in the data
Practice3 <- read_csv("C:/Users/Morgan/Desktop/Practice3.csv")
#Data must be as a dataframe
Practice3 <- as.data.frame(Practice3)
#Need to change the rownames to the species rather than numbers
Practice4 <- Practice3[,-1]
rownames(Practice4) <- Practice3[,1]
#Check the dataframe out to make sure all is well
str(Practice4)

#RUN THE TEST
out4 <- iNEXT(Practice4, q = c(0, 1, 2), datatype = "abundance")

#Make pretty graphs
ggiNEXT(out4, type = 1, facet.var = "site")
ggiNEXT(out4, type = 1, facet.var = "order")
