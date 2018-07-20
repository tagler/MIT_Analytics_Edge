dailykos <- read.csv("dailykos.csv")

distances <- dist(dailykos, method = "euclidean")
clusterdailykos <- hclust(distances, method = "ward.D") 
plot(clusterdailykos)

clusterGroups <- cutree(clusterdailykos, k = 7)

HierCluster1 <- subset(dailykos, clusterGroups == 1)
HierCluster2 <- subset(dailykos, clusterGroups == 2)
HierCluster3 <- subset(dailykos, clusterGroups == 3)
HierCluster4 <- subset(dailykos, clusterGroups == 4)
HierCluster5 <- subset(dailykos, clusterGroups == 5)
HierCluster6 <- subset(dailykos, clusterGroups == 6)
HierCluster7 <- subset(dailykos, clusterGroups == 7)

tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))

set.seed(1000)
KMC = kmeans(dailykos, centers = 7)
table(KMC$cluster)

KmeansCluster1 = subset(dailykos, KMC$cluster == 1)
KmeansCluster2 = subset(dailykos, KMC$cluster == 2)
KmeansCluster3 = subset(dailykos, KMC$cluster == 3)
KmeansCluster4 = subset(dailykos, KMC$cluster == 4)
KmeansCluster5 = subset(dailykos, KMC$cluster == 5)
KmeansCluster6 = subset(dailykos, KMC$cluster == 6)
KmeansCluster7 = subset(dailykos, KMC$cluster == 7)

KmeansCluster = split(dailykos, KMC$cluster)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

table(clusterGroups, KMC$cluster)

