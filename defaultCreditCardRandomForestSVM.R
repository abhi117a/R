library(xlsx)
defaultData <- read.xlsx2("default credit card clients.xls", sheetIndex =1)
head(defaultData)
str(defaultData)

defaultData$X1 <- as.numeric(defaultData$X1)
defaultData$X2 <- as.factor(defaultData$X2)
defaultData$X3 <- as.factor(defaultData$X3)
defaultData$X4 <- as.factor(defaultData$X4)
defaultData$X5 <- as.factor(defaultData$X5)
defaultData$X6 <- as.factor(defaultData$X6)
defaultData$X7 <- as.factor(defaultData$X7)
defaultData$X8 <- as.factor(defaultData$X8)
defaultData$X9 <- as.factor(defaultData$X9)
defaultData$X10 <- as.factor(defaultData$X10)
defaultData$X11 <- as.factor(defaultData$X11)
defaultData$X12 <- as.numeric(defaultData$X12)
defaultData$X13 <- as.numeric(defaultData$X13)
defaultData$X14 <- as.numeric(defaultData$X14)
defaultData$X15 <- as.numeric(defaultData$X15)
defaultData$X16 <- as.numeric(defaultData$X16)
defaultData$X17 <- as.numeric(defaultData$X17)
defaultData$X18 <- as.numeric(defaultData$X18)
defaultData$X19 <- as.numeric(defaultData$X19)
defaultData$X20 <- as.numeric(defaultData$X20)
defaultData$X21 <- as.numeric(defaultData$X21)
defaultData$X22 <- as.numeric(defaultData$X22)
defaultData$X23 <- as.numeric(defaultData$X23)
defaultData$Y <- as.factor(defaultData$Y)

library(ggplot2)
g <- ggplot(defaultData)
i = {}
for (i in colnames(defaultData)){
  print (g+geom_bar(aes(i)))
}

defaultData$X5 <- NULL

library(caTools)
split = sample.split(defaultData$Y, SplitRatio = 0.8)
defaultTrain <- subset(defaultData, split == T)
defaultTest <- subset(defaultData, split == F)

nrow(defaultTrain)
nrow(defaultTest)
ncol(defaultTest)



library(randomForest)
rfModel <- randomForest(Y~., data = defaultTrain, importance=TRUE)
summary(rfModel)
rdPred <- predict(rfModel, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,rdPred)
cm
nrow(defaultTest)

#SVM
library(e1071)
svmLin <- svm(Y~., data = defaultTrain, kernel = "linear", cost = 1000, scale = FALSE)
SVMPredLin <- predict(svmLin, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,SVMPredLin)
cm
nrow(defaultTest)

#Scaling
svmLinS <- svm(Y~., data = defaultTrain, kernel = "linear", cost = 1000, scale = TRUE)
SVMPredLinS <- predict(svmLinS, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,SVMPredLinS)
cm
nrow(defaultTest)

#kernel = polynomial
svmPoly <- svm(Y~., data = defaultTrain, kernel = "polynomial", cost = 5, gamma = 0.043)
SVMPredPoly <- predict(svmPoly, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,SVMPredPoly)
cm
nrow(defaultTest)
#reducing cost even further
svmPoly <- svm(Y~., data = defaultTrain, kernel = "polynomial", cost = 1, gamma = 0.043)
SVMPredPoly <- predict(svmPoly, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,SVMPredPoly)
cm
nrow(defaultTest)
#didn't improve the result

#trying kernel radial
svmRadial <- svm(Y~., data = defaultTrain, kernel = "radial", cost = 1, gamma = 0.043)
SVMPredRad <- predict(svmRadial, newdata = defaultTest[,-23], type = "class")
cm <- table(defaultTest$Y,SVMPredRad)
cm
nrow(defaultTest)

#same accuracy with radial and polynomial, for linear kernel the accuracy is very low 50% and it takes hours to run the code