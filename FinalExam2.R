bank <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/bank.csv")
str(bank)
mean(bank$age)

boxplot(bank$duration~bank$job)
summary(bank)

sort(tapply(bank$duration,bank$job,mean))

cor(bank)
cor(bank$emp.var.rate,bank$nr.employed)
cor(bank$cons.price.idx,bank$cons.conf.idx)
cor(bank$cons.conf.idx,bank$euribor3m)
cor(bank$nr.employed,bank$cons.conf.idx)
str(bank)

newBank <- dplyr::select(bank,emp.var.rate,cons.price.idx,cons.conf.idx,euribor3m,nr.employed)
min(cor(newBank))

library(caTools)
set.seed(201)
split <- sample.split(bank$y,SplitRatio = 0.7)

train <- subset(bank,split == TRUE)
test <- subset(bank,split == FALSE)

logModel <- glm(y~.-duration-euribor3m-nr.employed, data = train, family = "binomial")
summary(logModel)

logModelPred <- predict(logModel,newdata = test,type = "response")
table(test$y,logModelPred>0.5)

table(train$y)

logROCPred <- predict(logModel,newdata = test)
library(ROCR)

ROCRpredTest <-  prediction(logROCPred, test$y)

auc <-  as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
pred <- performance(logROCPred[,2],test$y)
perf <- performance(logROCPred[,2], "tpr","fpr")
plot(perf)

predictROC <- predict(logModel, newdata = test)
predictROC

pred <- prediction(predictROC[,2],test$y)
perf <- performance(pred, "tpr","fpr")
plot(perf,colorize = TRUE)

#calculating the AUC
AUC <- as.numeric(performance(pred, "auc")@y.values)
AUC


set.seed(201)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

tr.control <- trainControl(method = "cv", number = 10)
cartGrid <-  expand.grid( .cp = seq(0.001,0.05,0.001))

tr <- train(y~.-duration-euribor3m-nr.employed, data = train,method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

cartModel <- rpart(y~.-duration-euribor3m-nr.employed, method = "class",data = train,cp = 0.016)
prp(cartModel)

cartModelPred <- predict(cartModel,newdata = test, type = "class")
