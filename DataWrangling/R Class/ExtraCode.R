

simpbees <- subset(bees[,c(1:2,4)])
simpbees2 <- aggregate(Bees ~ Site, data = simpbees, FUN = sum)
simpbees2[1,1] <- "Total"
