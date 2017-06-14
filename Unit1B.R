library(readr)
BoeingStock <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/BoeingStock.csv", stringsAsFactors = FALSE)
CocaColaStock <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/CocaColaStock.csv", stringsAsFactors = FALSE)
GEStock <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/GEStock.csv", stringsAsFactors = FALSE)
IBMStock <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/IBMStock.csv", stringsAsFactors = FALSE)
ProcterGambleStock <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/ProcterGambleStock.csv", stringsAsFactors = FALSE)

str(BoeingStock)

IBMStock$Date = as.Date(IBMStock$Date, "%m/%d/%y")

GEStock$Date = as.Date(GEStock$Date, "%m/%d/%y")

CocaColaStock$Date = as.Date(CocaColaStock$Date, "%m/%d/%y")

ProcterGambleStock$Date = as.Date(ProcterGambleStock$Date, "%m/%d/%y")

BoeingStock$Date = as.Date(BoeingStock$Date, "%m/%d/%y")

nrow(IBMStock)
str(IBMStock)
summary(IBMStock)
min(GEStock$StockPrice)
max(CocaColaStock$StockPrice)
median(BoeingStock$StockPrice)
sd(ProcterGambleStock$StockPrice)
plot(CocaColaStock$Date, CocaColaStock$StockPrice, type = "l", col = "red")
lines(ProcterGambleStock$Date, ProcterGambleStock$StockPrice, col = "blue")

plot(CocaColaStock$Date[301:432], CocaColaStock$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBMStock$Date[301:432], IBMStock$StockPrice[301:432], col="blue")
lines(GEStock$Date[301:432], GEStock$StockPrice[301:432], col="purple")
lines(BoeingStock$Date[301:432], BoeingStock$StockPrice[301:432], col="green")
lines(ProcterGambleStock$Date[301:432], ProcterGambleStock$StockPrice[301:432], col="black")
tapply(IBMStock$StockPrice, months(IBMStock$Date),mean)
mean(IBMStock$StockPrice)
tapply(GEStock$StockPrice, months(GEStock$Date),mean)


