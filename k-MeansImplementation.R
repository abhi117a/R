#k-Means Clustering

kmean <- read.csv("Mall_Customers.csv")
head(kmean)
rownames(kmean)
colnames(kmean)
x <- kmean[4:5]
#Using Elbow method to find the optimal number of clusters
set.seed(6)
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(x,i)$withinss)
}
plot(1:10,wcss,type = "b",xlab = "Number of Clusters", ylab = "WCSS")

#Applying K-means 
set.seed(29)
kmeans <- kmeans(x,5, iter.max = 300, nstart = 10)

#Visuallizing
library(cluster)
clusplot(x,kmeans$cluster,lines =0,shade = T,color = T,labels = 2, plotchar = F, span = T, main = "Cluster k-Means", ylab = "Spending Power", xlab = "Annual Income")


