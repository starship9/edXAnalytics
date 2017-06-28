ClaimsData <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/ClaimsData.csv")
str(ClaimsData)
table(ClaimsData$bucket2009)/nrow(ClaimsData)
library(caTools)
set.seed(88)
split <- sample.split(ClaimsData$bucket2009, SplitRatio = 0.6)
claimsTrain <- subset(ClaimsData, split == TRUE)
claimsTest <- subset(ClaimsData, split == FALSE)
mean(claimsTrain$age)
table(claimsTrain$diabetes)
table(claimsTest$bucket2009, claimsTest$bucket2008)
accuracy <- (110138+10721+2774+1539+104)/nrow(claimsTest)
accuracy

#penalty matrix
penaltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE, nrow = 5)
penaltyMatrix

#Penalty error
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*penaltyMatrix)/nrow(claimsTest)
library(rpart)
library(rpart.plot)

claimsTree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = claimsTrain, method = "class", cp = 0.00005, parms = list(loss = penaltyMatrix))
prp(claimsTree)

predictTest <- predict(claimsTree, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, predictTest)
# (114141 + 16102 + 118 + 201 + 0)/nrow(claimsTest)
sum(as.matrix(table(claimsTest$bucket2009, predictTest))*penaltyMatrix)/nrow(claimsTest)
