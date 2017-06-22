framingham <-
  read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/framingham.csv",
           stringsAsFactors = FALSE)
str(framingham)
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train <- subset(framingham, split == TRUE)
test <- subset(framingham, split == FALSE)

framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)
predictTest <- predict(framinghamLog, type = "response", newdata = test)
table(test$TenYearCHD, predictTest > 0.5)
accuracy <- (1069 + 11)/(1069+6+187+11)
accuracy
library(ROCR)
ROCRPr <- prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRPred,"auc")@y.values)
