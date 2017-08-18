orders <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/orders.csv")
str(orders)

sort(table(orders$order_hour_of_day))
mean(orders$days_since_prior_order)

cor(orders$fresh.fruits,orders$fresh.vegetables)
table(orders$frozen.pizza)

orders.aisle <-  orders[, 5:ncol(orders)]
library(caret)
library(e1071)

preProc <- preProcess(orders.aisle)

ordersNorm <- predict(preProc,orders.aisle)
max(ordersNorm$frozen.dessert)
min(ordersNorm$soft.drinks)

distances <- dist(ordersNorm, method = "euclidean")
ClusterProducts <- hclust(distances, method = "ward.D")
plot(ClusterProducts, labels = FALSE)

set.seed(200)
kmc <- kmeans(ordersNorm, centers = 4)
kmc$cluster

str(kmc)
length(kmc$cluster)
tail(sort(colMeans(kmc)))
min(kmc$size)
max(kmc$size)

table(kmc$cluster)

library(wordcloud)
#wordcloud(kmc$cluster[kmc$cluster==1])
kmc$cluster[kmc$cluster==1]
kmc$cluster[kmc$cluster==2]
kmc$cluster[kmc$cluster==3]
kmc$cluster[kmc$cluster==4]

tapply(kmc$centers,kmc$cluster,summary)
tapply(kmc$cluster,kmc$centers,summary)

max(ordersNorm[kmc$cluster==1,])

tapply(kmc$centers,kmc$cluster,summary)
dim(kmc$centers)
length(kmc$cluster)

kmc$centers

tapply(ordersNorm$frozen.dessert,kmc$cluster,mean)
#tapply(AirlinesCluster$Balance, normClusters, mean)
length(kmc$cluster[kmc$cluster==4])
sort(rowSums(kmc$centers))

str(ordersNorm)
str(orders)

sort(rowSums(table(orders$order_hour_of_day,kmc$cluster)))
mean(table(orders$order_hour_of_day,kmc$cluster))

tapply(orders$days_since_prior_order,kmc$cluster,mean)
