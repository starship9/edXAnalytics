library(ggplot2)
library(ggmap)
library(maps)

statesMap <- map_data("state")
str(statesMap)
table(statesMap$group)

ggplot(data=statesMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(fill="white",color="black")

polling <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/PollingImputed.csv")
str(polling)

train <- subset(polling, Year==2004 | Year==2008)
str(train)
test <- subset(polling, Year==2012)
str(test)

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data = train, family = "binomial")
mod2Pred <- predict(mod2,newdata = test, type = "response")
mod2PredBin <- as.numeric(mod2Pred>0.5)

predDF <- data.frame(mod2Pred,mod2PredBin,test$State)
str(predDF)
max(predDF$mod2Pred)
min(predDF$mod2Pred)
table(predDF$mod2PredBin)
sum(table(predDF$test.State))
mean(predDF$mod2Pred)

#for easier merging
predDF$region <- tolower(predDF$test.State)
predMap <- merge(statesMap,predDF,by="region")
predMap <- predMap[order(predMap$order),]
dim(predMap)
dim(statesMap)

#plotting the predictions
ggplot(data = predMap,mapping = aes(x = long, y = lat, group = group,fill = mod2PredBin)) + geom_polygon(color = "black")

#discrete outcomes only
ggplot(data = predMap,mapping = aes(x = long, y = lat, group = group, fill = mod2PredBin))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

predMap$mod2Pred[predMap$region=="florida"]

#experimenting with geom_polygon()
ggplot(data = predMap,mapping = aes(x = long, y = lat, group = group, fill = mod2PredBin))+ geom_polygon(color = "black",linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(data = predMap,mapping = aes(x = long, y = lat, group = group, fill = mod2PredBin))+ geom_polygon(color = "black",size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(data = predMap,mapping = aes(x = long, y = lat, group = group, fill = mod2PredBin))+ geom_polygon(color = "black",alpha = 0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
