datos1 <- read.csv("/code/breastCancerData.csv", header = T)
tumors <- as.data.frame(datos1)
print(tumors)
print(tumors$Quimio)

c(names(tumors))


kmeans.result <- kmeans(data[, 1:9], 2)
table(tumors$Quimio, kmeans.result$cluster)
plot(tumors[c(names(tumors))], col = kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3,pch = 8, cex=2)

###-----------------------------------------------------------------------------------------------
# USING GGPLOT2
###-----------------------------------------------------------------------------------------------
library(datasets)
head(iris)
#install.packages(ggplot2)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
table(irisCluster$cluster, iris$Species)
irisCluster$cluster <- as.factor(irisCluster$cluster)
irisCluster$cluster
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
print(irisCluster$cluster)
print(irisCluster$centers)
print(irisCluster$size)

#-----------------------------------------------------------------------------------------
#Hierachical Clustering
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
irisSample$Species <- NULL
hc <- hclust(dist(irisSample), method="ave")
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
groups <- cutree(hc, k=3)


#---------------------------------------------------------------------------------
#Distance based clustering
library(fpc)
iris2 <- iris[-5] # remove class tags
ds <- dbscan(iris2, eps=0.42, MinPts=5)
# compare clusters with original class labels
table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])         
plotcluster(iris2, ds$cluster)
