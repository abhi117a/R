myData = read.csv("winequality-white.csv")
myData = data.frame(myData)

myData = myData[complete.cases(myData),]
myData$quality[myData$quality<=2] <- 90
myData$quality[myData$quality<=5] <- 80
myData$quality[myData$quality<=8] <- 70
myData$quality[myData$quality<=10] <- 60

myData$quality[myData$quality==80] <- "Not Bad"
myData$quality[myData$quality==90] <- "Bad"
myData$quality[myData$quality==70] <- "Good"
myData$quality[myData$quality==60] <- "Excellent"

myWineData = myData[,-12]


#elbow method
k.max <- 15
data <- myWineData
wss <- sapply(1:k.max,
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#looks like we should do 4 clusters
km_wine = kmeans(data,4)

h_wine = hclust(dist(data),method="ward.D2")
plot(h_wine)

#so looks like 4 clusters here as well

#now some pca
pca_wine = prcomp(data)
biplot(pca_wine,cex=c(1/3,1/2), scale=0)

# Let's plot the PVE
plot(pca_wine)


# We can compute the PVE as follows:
pca_wine.var =pca_wine$sdev ^2
pve=pca_wine.var/sum(pca_wine.var )
pve
# Here's the plot ...
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')
# Another view ...
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')