quality <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/quality.csv", stringsAsFactors = FALSE)
str(quality)
plot(quality$OfficeVisits, quality$Narcotics)
table(quality$PoorCare)
library(caTools)

set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
split
qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)

#type = "response" gives us probabilities
predictTrain <- predict(QualityLog, type = "response")
summary(predictTrain)

#higher probability for actual cases
tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog2 <- glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog2)
table(qualityTrain$PoorCare, predictTrain > 0.5)

library(ROCR)
ROCRPred <- prediction(predictTrain, qualityTrain$PoorCare)

#axes of the ROC curve
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))

predictTest <- predict(QualityLog,type = "response", newdata = qualityTest)
#for the area under the curve (measures accuracy)

ROCRpredTest <-  prediction(predictTest, qualityTest$PoorCare)

auc <-  as.numeric(performance(ROCRpredTest, "auc")@y.values)