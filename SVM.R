#SVm implementation

svmData <- read.csv("Social_Network_Ads.csv", na.strings = "")
head(svmData)
str(svmData)

svmData <- svmData[,3:5]

library(caTools)
set.seed(12345)
split <- sample.split(svmData$Purchased, SplitRatio = 0.75)
svm_training <- subset(svmData, split = T)
svm_test <- subset(svmData, split = F)

#FeatureScaling
svm_training[,1:2] <- scale(svm_training[,1:2])
svm_test[,-3] <- scale(svm_test[,-3])

#Classification
library(e1071)
classifier <- svm(formula = Purchased ~., data = svm_training,type = "C-classification", kernel = "radial")

#Predicting

y_pred <- predict(classifier,newdata = svm_test[,-3])

#ConfusionMatrix

cm <- table(svm_test[,3],y_pred)

