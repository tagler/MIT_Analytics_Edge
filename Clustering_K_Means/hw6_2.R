airlines <- read.csv("AirlinesCluster.csv")

library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)

distances <- dist(airlinesNorm, method = "euclidean")
clusterair <- hclust(distances, method = "ward.D") 
plot(clusterair)

clusterGroups <- cutree(clusterair, k = 5)
tapply(airlines$Balance, clusterGroups, mean)
hcluster1 <- subset(airlines, clusterGroups == 1)

lapply(split(airlines, clusterGroups), colMeans)

set.seed(88)
KMC = kmeans(airlines, centers = 5, iter.max = 1000)
table(KMC$cluster)

KMC$centers
