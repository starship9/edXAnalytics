pisa2009train <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/pisa2009train.csv", stringsAsFactors = FALSE)
pisa2009test <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/pisa2009test.csv", stringsAsFactors = FALSE)

str(pisa2009train)
tapply(pisa2009train$readingScore, pisa2009train$male, mean)

summary(pisa2009train)

pisa2009train = na.omit(pisa2009train)
pisa2009test = na.omit(pisa2009test)

nrow(pisa2009train)
nrow(pisa2009test)

pisa2009train$raceeth <- as.factor(pisa2009train$raceeth)
pisa2009test$raceeth <- as.factor(pisa2009test$raceeth)


#Using reference levels for factor variables used in regression
pisa2009train$raceeth <- relevel(pisa2009train$raceeth,"White")
pisa2009test$raceeth <- relevel(pisa2009test$raceeth,"White")

lmScore <- lm(readingScore~., data = pisa2009train)
summary(lmScore)

SSE4 <- sum(lmScore$residuals^2)
RMSE4 <- sqrt(SSE4/nrow(pisa2009train))

RMSE4

predTest <- predict(lmScore, newdata = pisa2009test)
summary(predTest)

SSEPred <- sum((predTest - pisa2009test$readingScore)^2)
RMSEPred <- sqrt(SSEPred/nrow(pisa2009test))

SSEPred
RMSEPred

mean(pisa2009train$readingScore)

SST <- sum((mean(pisa2009train$readingScore) - pisa2009test$readingScore)^2)
SST

#For displaying the R-squared value
1 - SSEPred/SST
