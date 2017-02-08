setwd("~/ISU/Project/Data/Bees")

beefamilies <- read.csv("2016 Bee IDs.csv")

table(beefamilies$Family)

props <- table(beefamilies$Family)/sum(table(beefamilies$Family))

ggplot(beefamilies, aes(x = Family)) + 
  geom_bar() + 
  facet_wrap( ~ Site)

ggplot(beefamilies, aes(x = Family, fill = Site)) + 
  geom_bar() +
  ggtitle("Bee Families by Site") +
  labs(y = Count)
#START HERE MORGAN
#MAKE BEE FAMILY BAR GRAPH