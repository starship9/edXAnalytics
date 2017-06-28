stevens <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/stevens.csv")
str(stevens)

library(caTools)
set.seed(3000)

split <- sample.split(stevens$Reverse, SplitRatio = 0.7)
training <- subset(stevens, split == TRUE)
testing <- subset(stevens,split==FALSE)

library(rpart)
library(rpart.plot)

stevensTree <- rpart(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, method = "class", minbucket = 25)
prp(stevensTree)

predictCart <- predict(stevensTree, newdata = testing, type = "class")

#Confusion matrix
table(testing$Reverse, predictCart)

library(ROCR)
predictROC <- predict(stevensTree, newdata = testing)
predictROC

pred <- prediction(predictROC[,2],testing$Reverse)
perf <- performance(pred, "tpr","fpr")
plot(perf)

#calculating the AUC
AUC <- as.numeric(performance(pred, "auc")@y.values)
AUC

stevensTree2 <- rpart(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, method = "class", minbucket = 5)
prp(stevensTree2)

stevensTree3 <- rpart(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, method = "class", minbucket = 100)
prp(stevensTree3)

library(randomForest)

training$Reverse <- as.factor(training$Reverse)
testing$Reverse <- as.factor(testing$Reverse)
stevensForest <- randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, nodesize = 25, ntree = 200)
predictForest <- predict(stevensForest, newdata = testing)
table(testing$Reverse, predictForest)

set.seed(100)

stevensForest1 <- randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, nodesize = 25, ntree = 200)
predictForest1 <- predict(stevensForest, newdata = testing)
table(testing$Reverse, predictForest1)

set.seed(200)
stevensForest2 <- randomForest(Reverse~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, nodesize = 25, ntree = 200)
predictForest2 <- predict(stevensForest, newdata = testing)
table(testing$Reverse, predictForest2)

library(caret)
library(e1071)

numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

stevensTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = training, method = "class", cp = 0.19)
predCV <- predict(stevensTreeCV, newdata = testing, type = "class")
table(testing$Reverse, predCV)
