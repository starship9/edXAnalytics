parole <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/parole.csv", stringsAsFactors = FALSE)
str(parole)
nrow(parole[parole$violator==1,])

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

str(parole)
summary(parole)

set.seed(144)
library(caTools)
split <- sample.split(parole$violator, SplitRatio = 0.7)
train <- subset(parole, split == TRUE)
test <- subset(parole, split == FALSE)

model1 <- glm(violator~., data = train, family = "binomial")
summary(model1)

pred1 <- predict(model1, type = "response", newdata = test)
max(pred1)
table(pred1 > 0.5, test$violator)

library(ROCR)
ROCRPr <- prediction(pred1, test$violator)
as.numeric(performance(ROCRPr,"auc")@y.values)
