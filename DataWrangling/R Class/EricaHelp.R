bees <- read.csv("Data/bees/raw/2016Bees.csv", header=T)

Site <- bees$Site[1:123]
BeeCount <- bees1$Bees[1:123]

tab <- cbind(Site,BeeCount)
tab

plot(tab)

test <- glm(BeeCount~Site)
summary(test)
