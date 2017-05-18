#---------------------------------------#
#  Percent Bare Ground ~ Bee Abundance  #
#---------------------------------------#

#Clear environment and set working directory
rm(list=ls())
setwd("~/ISU/Project/Previous Data/Data Files")

#Read in the data
et <- read.csv("ETrap_Bees.csv")

#Load libraries
library(ggplot2)

#Year column in "et" dataframe is brought in as an integer. Change to numeric.
pch.list <- as.numeric(et$Year)
pch.list

et$Year <- as.factor(et$Year)

#Plot BareGround vs. Ind (Percent Bare Ground vs. Bee Abundance)
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
#####Why have two models? How to interpret the second one?

#Morgan's plot using ggplot2
ggplot(et, aes(x = BareGround, y = Ind)) +
  geom_point(aes(size = 2, shape = Year, color = Year)) +
  #stat_smooth(method = "lm") +
  geom_abline(intercept = -0.7025809, slope = 0.3164081) +
  theme_bw() +
  labs(x = "Percent Bare Ground", y = "Bee Abundance") +
  ggtitle("Influence of Bare Ground on Bee Abundance") +
  theme(plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0))) +
  theme(legend.text = element_text(size = 10))

plot(et$BareGround, et$Ind,
     xlab = "Percent Bare Ground", ylab = "Bee Abundance")
abline(MMmodel)
abline(model)

MMmodel <- lm(Ind ~ BareGround, data = et)
summary(MMmodel)
