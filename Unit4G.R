data(state)
statedata = data.frame(state.x77)

str(statedata)

linReg <- lm(Life.Exp ~ . , data = statedata)
summary(linReg)

linRegPred <- predict(linReg)
linSSE <- sum((statedata$Life.Exp - linRegPred)^2)
linSSE

linReg2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(linReg2)

linRegPred2 <- predict(linReg2)
linSSE2 <- sum((statedata$Life.Exp - linRegPred2)^2)
linSSE2

library(rpart)
cart <- rpart(Life.Exp ~ ., data = statedata)
library(rpart.plot)
prp(cart)

cartPred <- predict(cart)
cartPredSSE <- sum((statedata$Life.Exp - cartPred)^2)
cartPredSSE
cart2 <- rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(cart2)

cartPred2 <- predict(cart2)
cartPredSSE2 <- sum((statedata$Life.Exp - cartPred2)^2)
cartPredSSE2

cart3 <- rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
cartPred3 <- predict(cart3)
cartPredSSE3 <- sum((statedata$Life.Exp - cartPred3)^2)
cartPredSSE3

library(caret)
library(e1071)

set.seed(111)

tr.control <- trainControl(method = "cv", number = 10)
cartGrid <-  expand.grid( .cp = seq(0.01,0.50,0.01))

tr <- train(Life.Exp~ ., data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

cart4 <- rpart(Life.Exp~., data = statedata, cp = 0.12)
prp(cart4)

cartPred4 <- predict(cart4)
cartPredSSE4 <- sum((statedata$Life.Exp - cartPred4)^2)
cartPredSSE4

set.seed(111)
tr2 <- train(Life.Exp~ Area, data = statedata, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr2

cart5 <- rpart(Life.Exp ~ Area, data = statedata, cp = 0.02)
prp(cart5)

cartPred5 <- predict(cart5)
cartPredSSE5 <- sum((statedata$Life.Exp - cartPred5)^2)
cartPredSSE5
