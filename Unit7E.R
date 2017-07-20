households <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/households.csv")
str(households)

library(ggplot2)
library(reshape2)

head(melt(households, id = "Year"))

ggplot(data = melt(households, id = "Year"),mapping = aes(Year, y = value, color = variable)) + geom_line(size = 2) + geom_point(size = 5) + ylab("Percentage of households")
