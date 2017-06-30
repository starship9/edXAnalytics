boston <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/boston.csv", stringsAsFactors = FALSE)
str(boston)

plot(boston$LAT, boston$LON)
points(boston$LAT[boston$CHAS==1],boston$LON[boston$CHAS==1],col="blue",pch=19)
points(boston$LAT[boston$TRACT==3531],boston$LON[boston$TRACT==3531],col="red",pch=19)
summary(boston$NOX)
points(boston$LAT[boston$NOX>=0.55],boston$LON[boston$NOX>=0.55],col="green",pch=19)
summary(boston$MEDV)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.20],boston$LAT[boston$MEDV>=21.2],col="red",pch=19)
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)

latlonLM <- lm(MEDV ~ LAT + LON, data = boston)
summary(latlonLM)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2], col = "red", pch = 19)
latlonLM$fitted.values
points(boston$LON[latlonLM$fitted.values>=21.2], boston$LAT[latlonLM$fitted.values>=21.2],col="blue",pch="$")

library(rpart)
library(rpart.plot)
 
latlonTree <- rpart(MEDV ~ LAT + LON, data = boston)
prp(latlonTree)
fittedValues <- predict(latlonTree)
points(boston$LON[fittedValues>=21.2],boston$LAT[fittedValues>=21.2],col="blue",pch = "$")

latlonTreeNew <- rpart(MEDV~LAT + LON, data = boston, minbucket = 50)
plot(latlonTreeNew)
text(latlonTreeNew)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)

library(caTools)
set.seed(123)
split <- sample.split(boston$MEDV, SplitRatio = 0.7)
train <- subset(boston, split == TRUE)
test <- subset(boston,split==FALSE)
linReg <- lm(MEDV~LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linReg)

linReg.pred <- predict(linReg,newdata = test)
linReg.sse <- sum((linReg.pred - test$MEDV)^2)
linReg.sse

rpartTree <- rpart(MEDV~LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(rpartTree)
tree.pred <- predict(rpartTree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse

library(caret)
library(e1071)

tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = (0:10)*0.001)

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS +CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
