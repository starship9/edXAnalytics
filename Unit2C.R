NBA_train <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/NBA_train.csv", stringsAsFactors = FALSE)
NBA_test <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/NBA_test.csv", stringsAsFactors = FALSE)

str(NBA_train)
table(NBA_train$W, NBA_train$Playoffs)
NBA_train$PTSDiff <- NBA_train$PTS - NBA_train$oppPTS
plot(NBA_train$PTSDiff, NBA_train$W)

WinsReg <- lm(W~PTSDiff, data = NBA_train)
summary(WinsReg)

PointsReg <- lm(PTS~X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA_train)
summary(PointsReg)
PointsReg$residuals

#sum of squared errors
SSE <- sum(PointsReg$residuals^2)
SSE

#root mean squared error
RMSE <- sqrt(SSE/nrow(NBA_train))
RMSE

mean(NBA_train$PTS)

#removing insignificant variables

PointsReg2 <- lm(PTS~X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA_train)
summary(PointsReg2)

PointsReg3 <- lm(PTS~X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA_train)
summary(PointsReg3)

PointsReg4 <- lm(PTS~X2PA + X3PA + FTA + AST + ORB + STL, data = NBA_train)
summary(PointsReg4)

SSE4 <- sum(PointsReg4$residuals^2)
SSE4

RMSE4 <- sqrt(SSE4/nrow(NBA_train))
RMSE4

PointsPrediction <- predict(PointsReg4, newdata = NBA_test)

SSEPred <- sum((PointsPrediction - NBA_test$PTS)^2)
SST <- sum((mean(NBA_train$PTS) - NBA_test$PTS)^2)

R2 <- 1 - SSEPred/SST
R2

RMSEPred <- sqrt(SSEPred/nrow(NBA_test))
RMSEPred
