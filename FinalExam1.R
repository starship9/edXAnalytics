park_visits <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/park_visits.csv")
str(park_visits)

visits2016July <- subset(park_visits, Month == 7)
visits2016July <- subset(visits2016July,Year == 2016)
str(visits2016July)

table(visits2016July$ParkType)
visits2016July[which.max(visits2016July$laglogVisits),]
visits2016July[which.min(visits2016July$laglogVisits),]

max(mean(visits2016July$logVisits))
min(mean(visits2016July$logVisits))

visits2016July$Region[which.max(mean(visits2016July$logVisits))]
table(visits2016July$Region,visits2016July$logVisits)

sort(tapply(visits2016July$logVisits,visits2016July$Region,mean))

cor(visits2016July$cost, visits2016July$logVisits)

ys <- subset(park_visits,ParkName=="Yellowstone NP")
str(ys)

ysTS <- ts(ys$logVisits,start = c(2010,1),freq = 12)
plot(ysTS)

colSums(is.na(park_visits))

park_visits <- park_visits[rowSums(is.na(park_visits))==0,]
str(park_visits)

park_visits$Month <- as.factor(park_visits$Month)

library(caTools)
set.seed(100)

train <- subset(park_visits,Year<=2014)
test <- subset(park_visits,Year>=2015)

mod <- lm(logVisits~laglogVisits, data = train)
summary(mod)

modPred <- predict(mod,newdata = test)
str(modPred)

SSE <- sum((modPred - test$logVisits)^2)
SSE

SSR <- sum((mod$residuals)^2)
SSR

1-(SSR/SSE)

mod2 <- lm(logVisits~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data  = train)
summary(mod2)

mod2Pred <- predict(mod2, newdata = test)
SSE2 <- sum((test$logVisits - mod2Pred)^2)
SSE2

SSR2 <- sum((mod2$residuals)^2)
SSR2

1-SSR2/SSE2

library(rpart)
library(rpart.plot)

cartMod <- rpart(logVisits~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data  = train,cp = 0.05)
prp(cartMod)

cartPred <- predict(cartMod,newdata = test)
SSE3 <- sum((cartPred - test$logVisits)^2)
SST3 <- sum((mean(test$logVisits)-test$logVisits)^2)

1 - (SSE3/SST3)

library(caret)
library(e1071)

set.seed(201)

tr.control <- trainControl(method = "cv", number = 10)
cartGrid <-  expand.grid( .cp = seq(0.0001,0.005,0.0001))

tr <- train(logVisits~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data  = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

plot(tr)

cartModCV <- rpart(logVisits~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data  = train,cp = 1e-04)
prp(cartModCV)

cartCVPred <- predict(cartModCV,newdata = test)
SSE4 <- sum((cartCVPred - test$logVisits)^2)
SST4 <- sum((mean(test$logVisits)-test$logVisits)^2)

1 - (SSE4/SST4)

library(randomForest)


set.seed(201)

rfMod <- randomForest(logVisits~laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost,data = train)
