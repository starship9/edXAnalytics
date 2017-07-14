AirlinesCluster <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/AirlinesCluster.csv")
str(AirlinesCluster)
summary(AirlinesCluster)

library(caret)
library(e1071)

#preprocessing and normalization
preproc <- preProcess(AirlinesCluster)
airlinesNorm <- predict(preproc, AirlinesCluster)
summary(airlinesNorm)

normDist <- dist(airlinesNorm, method = "euclidean")
normClust <- hclust(normDist, method = "ward.D")
plot(normClust)

rect.hclust(normClust, k = 6)

normClusters <- cutree(normClust, k = 5)
str(normClusters)
length(normClusters[normClusters==1])
tapply(AirlinesCluster$Balance, normClusters, mean)

set.seed(88)
kmc <- kmeans(airlinesNorm,centers = 5,iter.max = 100)
str(kmc$cluster)
str(kmc)
table(kmc$cluster)
kmc$centers
mean(normClusters[normClusters==1])
