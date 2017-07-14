dailykos <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/dailykos.csv")
str(dailykos)

kosDist <- dist(dailykos, method = "euclidean")
kosClust <- hclust(kosDist, method = "ward.D")
plot(kosClust)

kosClusters <- cutree(kosClust, k = 7)
kosClusters

kos1 <- subset(dailykos, kosClusters==1)
kos2 <- subset(dailykos, kosClusters==2)
kos3 <- subset(dailykos, kosClusters==3)
kos4 <- subset(dailykos, kosClusters==4)
kos5 <- subset(dailykos, kosClusters==5)
kos6 <- subset(dailykos, kosClusters==6)
kos7 <- subset(dailykos, kosClusters==7)

# str(kos3)
# dim(kos1)
# dim(kos2)
# dim(kos3)
# dim(kos4)
# dim(kos5)
# dim(kos6)
# dim(kos7)

tail(sort(colMeans(kos1)))
tail(sort(colMeans(kos2)))
tail(sort(colMeans(kos3)))
tail(sort(colMeans(kos4)))
tail(sort(colMeans(kos5)))
tail(sort(colMeans(kos6)))
tail(sort(colMeans(kos7)))

#kmeans
set.seed(1000)
kmc <- kmeans(dailykos, centers = 7)
kmc$cluster

kmc1 <- subset(dailykos, kmc$cluster==1)
str(kmc1)
kmc2 <- subset(dailykos, kmc$cluster==2)
kmc3 <- subset(dailykos, kmc$cluster==3)
kmc4 <- subset(dailykos, kmc$cluster==4)
kmc5 <- subset(dailykos, kmc$cluster==5)
kmc6 <- subset(dailykos, kmc$cluster==6)
kmc7 <- subset(dailykos, kmc$cluster==7)
dim(kmc3)
dim(kmc1)

tail(sort(colMeans(kmc1)))
table(kmc$cluster,kosClusters)
