#LogisticRegression

data_logistic <- read.csv("Social_Network_Ads.csv")
head(data_logistic)

data_logistic <- data_logistic[,3:5]

library(caTools)
set.seed(12345)

#Data Splitting into Test Set and Training Set

spilt <- sample.split(data_logistic$Purchased, SplitRatio = 0.75)
training_sett <- subset(data_logistic, split = T)
test_sett <- subset(data_logistic,split = F)

#Feature Scaling

training_sett[,1:2] <- scale(training_sett[,1:2])
test_sett[,1:2] <- scale(test_sett[,1:2])

#Fitting logistic regression

classifier <- glm(formula = Purchased ~. , family = binomial, data = training_sett)

#Predicitng with the model built

prob_pred <- predict(classifier, type = "response", newdata = test_sett[-3])
y_predd <- ifelse(prob_pred > 0.5,1,0)

#Making Confusion Matrix

cm <- table(test_sett[,3],y_predd)

#Visualizing the result

# Visualising the Training set results

library(ElemStatLearn)
set = training_sett
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Classifier (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'green', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_sett
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green3', 'red3'))
