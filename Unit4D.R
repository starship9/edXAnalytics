gerber <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/gerber.csv")
str(gerber)
table(gerber$voting)
summary(gerber)
sum(gerber$civicduty)
sum(gerber$neighbors)
sum(gerber$self)
sum(gerber$hawthorne)

model1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = "binomial")
summary(model1)

model1Pred <- predict(model1, type = "response")
table(gerber$voting, model1Pred > 0.5)
model1Pred

table(gerber$voting)

library(ROCR)
ROCRPr <- prediction(model1Pred, gerber$voting)
as.numeric(performance(ROCRPr,"auc")@y.values)

model2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
prp(model2)

model3 <- rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data = gerber, cp = 0.0)
prp(model3)

model4 <- rpart(voting ~ control, data = gerber, cp = 0.0)
prp(model4, digits = 6)
abs(0.296638 - 0.34)

model5 <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(model5, digits = 6)

model6 <- glm(voting ~ control + sex, data = gerber, family = "binomial")
summary(model6)

Possibilities <-  data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model6, newdata=Possibilities, type="response")

model7 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(model7)
predict(model7, newdata = Possibilities, type = "response")
