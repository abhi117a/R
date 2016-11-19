#k-NN

library(ISLR)
library(Amelia)
str(Caravan)
#Checking for NA/empty values
missmap(Caravan, col = c("Yellow","Black"), legend = F)
#Scaling the Data
Caravan[,-86] <- scale(Caravan[,-86])
head(Caravan)

library(caTools)
split <- sample.split(Caravan[,86], SplitRatio = 0.75)
train_caravan <- subset(Caravan, split == T)
test_caravan <- subset(Caravan, split==F)
nrow(test_caravan)


###Elbow Method
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(Caravan[,-86],i)$withinss)
}
plot(1:10,wcss,type = "b",xlab = "Number of Clusters", ylab = "WCSS")


#####
library(class)


model <- knn(train_caravan[,-86], test_caravan[,-86], cl = train_caravan[,86], k=6)
summary(model)

#Confusion Matrix
cm <- table(test_caravan[,86],model)
acc <- (1355+5)/(1355+5+82+13)


