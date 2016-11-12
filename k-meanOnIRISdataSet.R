#####################k-mean#########################################
#data Visualization
head(iris)
library(ggplot2)
ggplot(data = iris, aes(x=iris$Petal.Length, y = iris$Petal.Width, color = iris$Species)) + 
  geom_point(size = 4)



#elbowMethod
set.seed(123)
wcss <- vector()
for(i in 1:10){
  wcss[i]<- sum(kmeans(iris[,1:4],i)$withinss)
}
plot(1:10,wcss,type = "b",xlab = "Number of Clusters", ylab = "WCSS")
###############################################
#ElbowMethod shows 2 clusters as optimum but as we know the dataset in prior we are going with three clusters. It shows that elbow method is just a helper method it is by no means a sure shot solution for selection of k
###############################################
#implemet k-mean

set.seed(101)
irisCLuster <- kmeans(iris[,1:4],3,iter.max = 300, nstart = 30)


#confusionMatrix
cm <- table(irisCLuster$cluster, iris$Species)

###K-mean Visualization
#clusplot explains the most 2 features which have most impact on the data.
library(cluster)
clusplot(iris,irisCLuster$cluster,lines = 0,shade = T, color = T, labels = 0, plotchar = F, span = T, main = "Cluster k-Means", ylab = "Width", xlab = "length")



