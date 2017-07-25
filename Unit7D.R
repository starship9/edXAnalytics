intl <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/intl.csv")
str(intl)

library(ggplot2)

#basic bar plot
ggplot(data = intl, mapping = aes(x = Region, y = PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label = PercentOfIntl))

#reordering using the transform function
intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

intl$PercentOfIntl <- intl$PercentOfIntl*100

ggplot(data = intl, mapping = aes(x = Region, y  = PercentOfIntl)) + geom_bar(stat = "identity", fill = "dark blue") + geom_text(aes(label = PercentOfIntl), vjust = -0.4) +ylab("Percentage of International Students") + theme(axis.title.x = element_blank(), axis.text.x =  element_text(angle = 45, hjust = 1))
                                                                                                                                                                                                                              
intlall <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/intlall.csv", stringsAsFactors = FALSE)
str(intlall)

library(ggmap)

intlall[is.na(intlall)] <- 0
str(intlall)

worldMap <- map_data("world")
str(worldMap)

worldMap <- merge(worldMap, intlall, by.x = "region", by.y = "Citizenship")
str(worldMap)

ggplot(data = worldMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

#reordering
worldMap <- worldMap[order(worldMap$group,worldMap$order),]
ggplot(data = worldMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")

table(intlall$Citizenship)

intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] <- "China"
ggplot(data = worldMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total.y), color = "black") + coord_map("mercator")
 
ggplot(data = worldMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total.y), color = "black") + coord_map("ortho",orientation = c(20,30,0))
ggplot(data = worldMap, mapping = aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total.y), color = "black") + coord_map("ortho",orientation = c(-37,175,0))
