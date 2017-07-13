healthy <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/healthy.csv", header = FALSE)
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length = 256)))

healthyVector <- as.vector(healthyMatrix)

distance <- dist(healthyVector, method = "euclidean")
str(healthyVector)

#kmeans
set.seed(1)
kmc <- kmeans(healthyVector, centers = 5, iter.max = 1000)
str(kmc)

healthyClusters <- kmc$cluster
kmc$centers

dim(healthyClusters) <- c(nrow(healthyMatrix),ncol(healthyMatrix))
image(healthyClusters, axes = FALSE, col = rainbow(5))

tumor <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/tumor.csv", header = FALSE)
tumorMatrix <- as.matrix(tumor)
tumorVector <- as.vector(tumorMatrix)

#for cluster analysis
library(flexclust)

#need to convert clusters to kcca objects for prediction
kmc.kcca <- as.kcca(kmc, healthyVector)
tumorClusters <- predict(kmc.kcca,newdata = tumorVector)

#conversion
dim(tumorClusters) <- c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters,axes = FALSE, col = rainbow(5))
