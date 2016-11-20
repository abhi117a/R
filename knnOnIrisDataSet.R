library(ISLR)
df <- iris
head(df)
str(df)
summary(df)
#scaling

df[,-5] <- scale(df[,-5])

#splitting
library(caTools)
split <- sample.split(df[,5], SplitRatio = 0.75)
train_df <- subset(df, split == T)
test_df <- subset(df, split==F)
nrow(train_df)

#elbow Method
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(df[,-5],i)$withinss)
}
plot(1:10,wcss,type = "b",xlab = "Number of Clusters", ylab = "WCSS")

#Modeling
library(class)
model <- knn(train = train_df[,-5], test = test_df[,-5], cl = train_df[,5], k =3)

#Confusion Matrix

cm <- table(test_df[,5],model)

library(ElemStatLearn)
set = test_df[,-5]
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
X3 = seq(min(set[, 3]) - 1, max(set[, 3]) + 1, by = 0.01)
X4 = seq(min(set[, 4]) - 1, max(set[, 4]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2, X3,X4)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = train_df[, -5], test = grid_set, cl = train_df[, 5], k = 3)
plot(set[, -5],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



