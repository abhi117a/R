library(ISLR)
dataC <- Carseats
head(Carseats)
library(ggplot2)
hist(dataC$Sales, main = "Sales")

#Running For loop to draw distribution plot for every column
i = {}
for(i in colnames(dataC)){
  print(i)
  hist(dataC[,i], main = i)
}

High <- ifelse(dataC$Sales >= 8, "Yes", "No")

dataC <- data.frame(dataC,High)
head(dataC)

library(caTools)
split = sample.split(dataC$Sales, SplitRatio = 0.8)
dataCtest <- subset(dataC, split== FALSE)
dataCtrain <- subset(dataC, split== TRUE)
nrow(dataCtest)
nrow(dataCtrain)

library(tree)

pred <- tree(High~.-Sales, data = dataCtrain)
summary(pred)

plot(pred)
text(pred,pretty = 0)
pred

fin <- predict(pred, newdata = dataCtest, type = "class")
cm <- table(dataCtest$High,fin)
cm
nrow(dataCtest)
print("Accuracy")
print(60/80)

#Pruning

CVfin <- cv.tree(pred,FUN = prune.misclass)
names(CVfin)
CVfin

par(mfrow= c(1,2))
plot(CVfin$size,CVfin$k, type = "b")
plot(CVfin$k,CVfin$dev, type = "b")


pruneVal = prune.misclass(pred,best = 16)
plot(pruneVal)
text(pruneVal, pretty = 0)


finValPruned <- predict(pruneVal, dataCtest[-"High"], type = "class")
  table(dataCtest$High, finValPruned)
