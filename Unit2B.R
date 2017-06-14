baseball <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/baseball.csv", stringsAsFactors = FALSE)
str(baseball)

moneyball <- subset(baseball, Year<2002)
str(moneyball)
moneyball$RD <- moneyball$RS - moneyball$RA
str(moneyball)

plot(moneyball$RD, moneyball$W)

WinsReg <- lm(W~RD, data = moneyball)
summary(WinsReg)
RunsReg <- lm(RS~OBP + SLG, data = moneyball)
summary(RunsReg)
teamRank <-  c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank,wins2012)
