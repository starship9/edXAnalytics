WHO <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)
library(ggplot2)
#fertilityGNIPlot<- ggplot(data = WHO, mapping = aes(x = GNI, y = FertilityRate)) + geom_point(col = "darkred",size = 3, shape = 15) + ggtitle("Fertility Rate vs GNI")
#pdf("fertilityPlot.pdf")
#print(fertilityGNIPlot)
#dev.off()

ggplot(data = WHO, mapping = aes(x = GNI, y = FertilityRate, color = Region)) + geom_point() + ggtitle("Fertility Rate vs GNI")
ggplot(data = WHO, mapping = aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point() + ggtitle("Fertility Rate vs GNI")
ggplot(data = WHO, mapping = aes(x = FertilityRate, y = Under15)) + geom_point()
#log + regression line
ggplot(data = WHO, mapping = aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

model <- lm(Under15 ~ log(FertilityRate), data = WHO)
summary(model)
