PollingData <-
  read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/PollingData.csv",
           stringsAsFactors = FALSE)
str(PollingData)

#for easily identifying the year(s) with missing data
table(PollingData$Year)
summary(PollingData)

#for imputation
library(mice)
simple <-
  dplyr::select(PollingData, Rasmussen, SurveyUSA, PropR, DiffCount)
names(simple)
str(simple)
summary(simple)
set.seed(144)

#fills in missing values
imputed <- complete(mice(simple))
summary(imputed)

PollingData$Rasmussen <- imputed$Rasmussen
PollingData$SurveyUSA <- imputed$SurveyUSA
summary(PollingData)

Train <- subset(PollingData, Year == 2008 | Year == 2004)
Test <- subset(PollingData, Year == 2012)
table(Train$Republican)
table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen))
cor(Train[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

repReg1 <- glm(Republican ~ PropR, data = Train, family = "binomial")
summary(repReg1)
pred1 <- predict(repReg1, type = "response")
table(Train$Republican, pred1 >= 0.5)

repReg2 <- glm(Republican ~ SurveyUSA + DiffCount, data = Train, family = "binomial")
summary(repReg2)
pred2 <- predict(repReg2, type = "response")
table(Train$Republican, pred2>=0.5)
table(Test$Republican, sign(Test$Rasmussen))

pred2 <- predict(repReg2, newdata = Test, type = "response")
table(Test$Republican,pred2 >= 0.5)

#incorrect subset
subset(Test, pred2 >= 0.5 & Republican==0)
