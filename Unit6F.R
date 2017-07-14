StocksCluster <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/StocksCluster.csv")
str(StocksCluster)
table(StocksCluster$PositiveDec)
min(cor(StocksCluster))
max(cor(StocksCluster)[cor(StocksCluster)!=1])
summary(StocksCluster)

library(caTools)
set.seed(144)
split <- sample.split(StocksCluster$PositiveDec, SplitRatio = 0.7)
train <- subset(StocksCluster, split == TRUE)
test <- subset(StocksCluster, split == FALSE)

posLogReg <- glm(PositiveDec~., data = train, family = "binomial")
trainPred <- predict(posLogReg, type = "response")
table(train$PositiveDec, trainPred>0.5)
testPred <- predict(posLogReg, newdata = test, type = "response")
table(test$PositiveDec, testPred>0.5)
table(test$PositiveDec)

#removing dependent variable for clustering
limitedTrain <- train
limitedTrain$PositiveDec = NULL
limitedTest <- test
limitedTest$PositiveDec = NULL

library(caret)
library(e1071)

#normalization
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km <- kmeans(normTrain, centers = 3)
table(km$cluster)

#for training/testing clustering assignment
library(flexclust)
km.kcca <- as.kcca(km,normTrain)
clusterTrain <- predict(km.kcca)
clusterTest <- predict(km.kcca, newdata = normTest)
clusterTest
table(clusterTest)

train1 <- subset(train, km$cluster==1)
train2 <- subset(train,km$cluster==2)
train3 <- subset(train, km$cluster==3)
test1 <- subset(test,km$cluster==1)
test2 <- subset(test,km$cluster==2)
test3 <- subset(test,km$cluster==3)
mean(train1$PositiveDec)
mean(train2$PositiveDec)
mean(train3$PositiveDec)

glm1 <- glm(PositiveDec~., data = train1, family = "binomial")
glm2 <- glm(PositiveDec~., data = train2, family = "binomial")
glm3 <- glm(PositiveDec~., data = train3, family = "binomial")

summary(glm1)
summary(glm2)
summary(glm3)

predictTest1 <- predict(glm1, newdata = test1, type = "response")
predictTest2 <- predict(glm2, newdata = test2, type = "response")
predictTest3 <- predict(glm3, newdata = test3, type = "response")

table(test1$PositiveDec, predictTest1>0.5)
table(test2$PositiveDec, predictTest2>0.5)
table(test3$PositiveDec, predictTest3>0.5)

AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcomes = c(test1$PositiveDec, test2$PositiveDec, test3$PositiveDec)

table(AllOutcomes, AllPredictions>0.5)
