elantra <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/elantra.csv", stringsAsFactors = FALSE)

str(elantra)

elantraTraining <- subset(elantra, Year<=2012)
elantraTesting <- subset(elantra, Year>2012)
nrow(elantraTraining)

salesReg <- lm(ElantraSales~Unemployment + CPI_energy + CPI_all + Queries, data = elantraTraining)
summary(salesReg)

salesReg2 <- lm(ElantraSales~Month + Unemployment + CPI_energy + CPI_all + Queries, data = elantraTraining)
summary(salesReg2)

elantra$Monthfactor <- as.factor(elantra$Month)

salesReg3 <- lm(ElantraSales~Monthfactor + Unemployment + CPI_energy + CPI_all + Queries, data = elantraTraining)
summary(salesReg3)

cor(elantraTraining)
class(elantraTraining)
names(elantraTraining)

elantraTraining <- subset(elantra, Year<=2012)
elantraTesting <- subset(elantra, Year>2012)
names(elantraTraining)
cor(elantraTraining)
cor(elantra)
salesReg3 <- lm(ElantraSales~Monthfactor + Unemployment + CPI_energy + CPI_all, data = elantraTraining)
summary(salesReg3)

salesPred <- predict(salesReg3, newdata = elantraTesting)
SSE <- sum((salesPred - elantraTesting$ElantraSales)^2)
SSE

mean(elantraTraining$ElantraSales)
SST <- sum((mean(elantraTraining$ElantraSales) - elantraTesting$ElantraSales)^2)
SST

sort(salesPred - elantraTesting$ElantraSales)
#For displaying the R-squared value
1 - SSE/SST

elantraTesting$Month[which.max(abs(salesPred - elantraTesting$ElantraSales))]
