library(readxl)
gardsil <- read_excel("gardasil.xls")
head(gardsil)
colnames(gardsil)
str(gardsil)
ncol(gardsil)

hist(gardsil$Age)
hist(gardsil$AgeGroup)
hist(gardsil$Race)
hist(gardsil$Race)
hist(gardsil$Shots)
hist(gardsil$Completed)
hist(gardsil$InsuranceType)
hist(gardsil$MedAssist)
hist(gardsil$Location)
hist(gardsil$LocationType)
hist(gardsil$PracticeType)
gardsil$Completed <- as.factor(gardsil$Completed)
library(caTools)
split = sample.split(gardsil$Completed, SplitRatio = 0.8)
gardsilTrain <- subset(gardsil, split== TRUE)
nrow(gardsilTrain)
gardsilTest <- subset(gardsil, split==FALSE)
nrow(gardsilTest)
colnames(gardsilTest)

library(randomForest)
rfModel <- randomForest(Completed~., data = gardsilTrain, importance=TRUE)
summary(rfModel)
rdPred <- predict(rfModel, newdata = gardsilTest[,-5], type = "class")
cm <- table(gardsilTest$Completed,rdPred)
cm
nrow(gardsilTest)

#SVM

library(e1071)
svmLin <- svm(Completed~., data = gardsilTrain, kernel = "linear", cost = 1, scale = FALSE)
svmLinPred <- predict(svmLin, newdata = gardsilTest[,-5])
cm <- table(gardsilTest$Completed,svmLinPred)
cm
nrow(gardsilTest)


svmLinS <- svm(Completed~., data = gardsilTrain, kernel = "linear", cost = 1, scale = TRUE)
svmLinPredS <- predict(svmLinS, newdata = gardsilTest[,-5])
cm <- table(gardsilTest$Completed,svmLinPredS)
cm
nrow(gardsilTest)


#SVM with CV

tuneSVM = tune(svm,Completed~., data=gardsilTest, kernel="linear", ranges=list(cost = c(.001, .01, .1, 1, 5, 10, 100)))
summary(tuneSVM)

bestmodle = tuneSVM$best.model
table(predict(bestmodle,gardsilTest[,-5]),gardsilTest$Completed)
############
#NON Linear
############

svmPol <- svm(Completed~., data = gardsilTrain, kernel = "polynomial", gamma = 0.1,cost = 1)
table(predict(svmPol,gardsilTest[,-5]),gardsilTest$Completed)

#Radial
svmRad <- svm(Completed~., data = gardsilTrain, kernel = "radial", gamma = 0.1,cost = 1)
table(predict(svmRad,gardsilTest[,-5]),gardsilTest$Completed)


