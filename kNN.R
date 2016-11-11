#kNN
knn <- read.csv("Social_Network_Ads.csv", na.strings = "")
head(knn)
knn <- knn[,3:5]
summary(knn)
str(knn)

knn$Purchased <- factor(knn$Purchased, levels = c(0,1))

library(caTools)
set.seed(12345)
split <- sample.split(knn$Purchased, SplitRatio = 0.75)
knn_trainig <- subset(knn, split == TRUE )
knn_test <- subset(knn, split == FALSE)

# Feature Scaling

knn_trainig[,1:2] <- scale(knn_trainig[,1:2])
knn_test[,-3] <- scale(knn_test[,-3])

library(class)
y_pred1 <- knn(train = knn_trainig[,-3], test = knn_test[,-3],cl = knn_trainig[,3] ,k = 5)

confusionMatrix <- table(knn_test[,3], y_pred1)


# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = knn_trainig[, -3], test = grid_set, cl = knn_trainig[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = knn_test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = knn_trainig[, -3], test = grid_set, cl = knn_trainig[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

