census <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/census.csv")
str(census)

library(caTools)
set.seed(2000)

split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)

logReg <- glm(over50k ~. , data = train, family = "binomial")
summary(logReg)

logRegPred <- predict(logReg, newdata = test, type = "response")
table(test$over50k, logRegPred > 0.5)
table(train$over50k)

library(ROCR)
ROCRPr <- prediction(logRegPred, test$over50k)
as.numeric(performance(ROCRPr,"auc")@y.values)

library(rpart)
cart <- rpart(over50k ~ ., data = train, method = "class")
cartPred <- predict(cart, newdata = test, type = "class")

library(rpart.plot)
prp(cart)

table(test$over50k, cartPred)
cartPred2 <- predict(cart, newdata = test)
cartPred2

perfReg <- performance(ROCRPr, "tpr", "fpr")
plot(perfReg)

str(cartPred2)
class(logRegPred)
class(cartPred2)

ROCRPr2 <- prediction(cartPred2[,2], test$over50k)
perfCart <- performance(ROCRPr2, "tpr","fpr")
plot(perfCart)
as.numeric(performance(ROCRPr2,"auc")@y.values)

set.seed(1)
trainSmall <-  train[sample(nrow(train), 2000), ]
library(randomForest)

set.seed(1)
rfModel <- randomForest(over50k ~ ., data = trainSmall)

rfModelPred <- predict(rfModel, newdata = test)
table(test$over50k, rfModelPred)

#number of times a variable is selected for splitting
vu <-  varUsed(rfModel, count=TRUE)
vusorted <-  sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rfModel$forest$xlevels[vusorted$ix]))

#impurity plot
varImpPlot(rfModel)

library(caret)
library(e1071)

set.seed(2)
tr.control <- trainControl(method = "cv", number = 10)
cartGrid <-  expand.grid( .cp = seq(0.002,0.1,0.002))

tr <- train(over50k ~ ., data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

cpCart <- rpart(over50k ~ ., data = train, method = "class", cp = 0.002)
cpCartPred <- predict(cpCart, newdata = test, type = "class")
table(test$over50k, cpCartPred)
prp(cpCart)
