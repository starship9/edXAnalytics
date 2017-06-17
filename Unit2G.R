data(state)
stateData <-  cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(stateData)

plot(stateData$x, stateData$y)

tapply(stateData$HS.Grad, stateData$state.region, mean)
boxplot(stateData$Murder~stateData$state.region)

subset(stateData, state.region == "Northeast")

lifeExpModel <- lm(Life.Exp~Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = stateData)
summary(lifeExpModel)

plot(stateData$Income, stateData$Life.Exp)

lifeExpModel <- lm(Life.Exp~Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = stateData)
summary(lifeExpModel)

lifeExpModel <- lm(Life.Exp~Population + Income + Murder + HS.Grad + Frost, data = stateData)
summary(lifeExpModel)

lifeExpModel <- lm(Life.Exp~Population + Murder + HS.Grad + Frost, data = stateData)
summary(lifeExpModel)

lifeExpPred <- predict(lifeExpModel)
sort(lifeExpPred)

stateData$state.name[which.min(stateData$Life.Exp)]
stateData$state.name[which.max(stateData$Life.Exp)]

sort(lifeExpModel$residuals)
