#k-means clustering 
df1 <- read.csv("winequality-white.csv", sep = ";") #Seprator is ";"
df2 <- read.csv("winequality-red.csv", sep = ";")

head(df1)
colnames(df2)


df1$Label <- "White"
df2$Label <- "red"

wine <- rbind(df1,df2)

wine[wine$Label == "red",]
str(wine)
library(ggplot2)
g <- ggplot(data = wine)
g +  geom_histogram(aes(x=wine$residual.sugar,fill = wine$Label),color = "black", binwidth = 1)+scale_fill_manual(values = c("#ae4554","white")) + theme_bw()

g+ geom_histogram(aes(x = wine$citric.acid, fill = wine$Label), color = "black") +scale_fill_manual(values = c("#ae4554","white")) + theme_bw()

g + geom_histogram(aes(x = wine$alcohol, fill = wine$Label), color = "black")+scale_fill_manual(values = c("#ae4554","white")) + theme_bw()

g + geom_point(aes(x = wine$residual.sugar, y = wine$citric.acid, color = wine$Label))+scale_color_manual(values = c("#ae4554","white")) + theme_dark()

g + geom_point(aes(x=wine$volatile.acidity, y = wine$residual.sugar, color = wine$Label))+scale_color_manual(values = c("#ae4554","white")) + theme_dark()


clus.data <- wine

clus.data$Label <- NULL
head(clus.data)

#applying kmeans data!!

set.seed(123)
#elbowMethod
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(clus.data,i)$withinss)
}
plot(1:10, wcss, type ="b",)

#implementing kmeansAlgorithm

wineKmeans <- kmeans(clus.data,3,iter.max = 300,nstart = 30)

#confusion matrix

cm <- table(wineKmeans$cluster,wine$Label)
#Visualizing the datat
library(cluster)
clusplot(wine,wineKmeans$cluster,lines = 0,shade = T, color = T, labels = 0, plotchar = T, span = T)
