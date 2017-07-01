letters_ABPR <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/letters_ABPR.csv")
str(letters_ABPR)

letters_ABPR$isB = as.factor(letters_ABPR$letter == "B")
library(caTools)
set.seed(1000)
split <- sample.split(letters_ABPR$isB, SplitRatio = 0.5)
train <- subset(letters_ABPR, split == TRUE)
test <- subset(letters_ABPR, split == FALSE)

table(letters_ABPR$isB)

library(rpart)
library(rpart.plot)
CARTb <-  rpart(isB ~ . - letter, data=train, method="class")
CARTbPred <- predict(CARTb, newdata = test,type = "class")
table(test$isB, CARTbPred)

library(randomForest)

set.seed(1000)
randomForestB <- randomForest(isB ~ . - letter, data=train)
randomForestBPred <- predict(randomForestB, newdata = test)
table(test$isB, randomForestBPred)

letters_ABPR$letter <- as.factor(letters_ABPR$letter)
set.seed(2000)
letterSplit <- sample.split(letters_ABPR$letter, SplitRatio = 0.5)
trainLetter <- subset(letters_ABPR, letterSplit==TRUE)
testLetter <- subset(letters_ABPR, letterSplit==FALSE)

table(trainLetter$letter)
table(testLetter$letter)

letterCart <- rpart(letter ~ .-isB, data = trainLetter, method = "class")
letterCartPred <- predict(letterCart, newdata = testLetter, type = "class")
table(testLetter$letter, letterCartPred)

set.seed(1000)
letterRF <- randomForest(letter ~ .-isB, data = trainLetter)
letterRFPred <- predict(letterRF, newdata = testLetter)
table(testLetter$letter, letterRFPred)
