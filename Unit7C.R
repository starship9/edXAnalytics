murders <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/murders.csv")
str(murders)

library(maps)
library(ggmap)
statesMap <- map_data("state")
str(statesMap)

ggplot(data = statesMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white",color = "black")

murders$region <- tolower(murders$State)

#merging two data frames
murderMap <- merge(statesMap, murders, by = "region")
str(murderMap)

#plotting, scale_fill defines the gradient
ggplot(data = murderMap, mapping = aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "white") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

#population map
ggplot(data = murderMap, mapping = aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "white") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

murderMap$murderRate <- (murderMap$Murders/murderMap$Population) * 100000

ggplot(data = murderMap, mapping = aes(x = long, y = lat, group = group, fill = murderRate)) + geom_polygon(color = "white") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

#gun ownership
ggplot(data = murderMap, mapping = aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "white") + scale_fill_gradient(low = "black", high = "red", guide = "legend")