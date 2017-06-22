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
