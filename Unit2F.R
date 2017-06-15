FluTrain <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/FluTrain.csv", stringsAsFactors = FALSE)
str(FluTrain)
tail(FluTrain)

#Week with the max number of queries
FluTrain$Week[which.max(FluTrain$ILI)]
FluTrain$Week[which.max(FluTrain$Queries)]
hist(FluTrain$ILI)

plot(log(FluTrain$ILI), FluTrain$Queries)

FluTrend1 <- lm(log(ILI)~Queries, data = FluTrain)
summary(FluTrend1)
cor(FluTrain$ILI,FluTrain$Queries)

FluTest <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/FluTest.csv", stringsAsFactors = FALSE)

#using exp() since the model uses a logarithm as a deoendent variable
PredTest1 <-  exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

RE <- ((-PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]) + (FluTest$ILI[11]))/FluTest$ILI[11]
RE
FluTest[11,]

SSERs <- sum((PredTest1 - FluTest$ILI)^2)
RMSERs <- sqrt(SSERs/nrow(FluTest))
RMSERs

#for timeseries analysis

library(zoo)
ILILag2 <-  lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 <-  coredata(ILILag2)
summary(FluTrain$ILILag2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

FluTrend2 <- lm(log(ILI)~Queries + log(ILILag2),data = FluTrain)
summary(FluTrend2)

ILILag2 <-  lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 <-  coredata(ILILag2)
summary(FluTest$ILILag2)

FluTest$ILILag2[1] <- FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[2] <- FluTrain$ILI[nrow(FluTrain)]
head(FluTest$ILILag2,2)

PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))
SSERs2 <- sum((PredTest2 - FluTest$ILI)^2)
RMSERs2 <- sqrt(SSERs2/nrow(FluTest))
SSERs2
RMSERs2
