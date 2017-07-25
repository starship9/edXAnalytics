parole <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/parole.csv",stringsAsFactors = FALSE)
str(parole)

parole$male <- as.factor(parole$male)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
table(parole$male, parole$violator)
table(parole$crime,parole$state)

library(ggplot2)

ggplot(data = parole, mapping = aes(x = age)) + geom_histogram(binwidth = 5,color="blue") + facet_grid(.~male)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, mapping = aes(x = age, fill = male)) + geom_histogram(binwidth = 5) + scale_fill_manual(values = colorPalette)

ggplot(data = parole, mapping = aes(x = age, fill = male)) + geom_histogram(binwidth = 5, position = "identity",alpha = 0.5) + scale_fill_manual(values = colorPalette)
  
ggplot(data = parole, mapping = aes(x = time.served,fill = crime)) + geom_histogram(binwidth = 1,position = "identity",alpha = 0.5)
