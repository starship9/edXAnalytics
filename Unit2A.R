wine <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/wine.csv", stringsAsFactors = FALSE)
wine_test <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/wine_test.csv", stringsAsFactors = FALSE)

str(wine)
summary(wine)

model1 <- lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals

SSE <- sum(model1$residuals^2)
SSE

model2 <- lm(Price~AGST+HarvestRain, data = wine)
summary(model2)

SSE2 <- sum(model2$residuals^2)
SSE2

model3 <- lm(Price~AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE3 <- sum(model3$residuals^2)
SSE3

model4 <- lm(Price~HarvestRain + WinterRain, data = wine)
summary(model4)

newModel3 <- lm(Price~AGST + HarvestRain + WinterRain + Age, data = wine)
summary(newModel3)

#For correlation

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

model5 <- lm(Price~AGST + HarvestRain + WinterRain, data = wine)
summary(model5)

str(wine_test)
predictTest <- predict(newModel3, newdata = wine_test)
predictTest

SSETest <- sum((wine_test$Price-predictTest)^2)
SSETest

SST <- sum((wine_test$Price - mean(wine$Price))^2)
1- (SSETest/SST)
