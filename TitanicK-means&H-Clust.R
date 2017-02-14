library(ade4)
library(vegan)
library(cluster)
library(klaR)

dist()
getwd()
setwd("C:/Users/admin/Documents")
titanicc <- read.csv("titanic.csv")
head(titanicc)
newTitanic <- cbind(titanicc$pclass,titanicc$survived, titanicc$sex, titanicc$age)
head(newTitanic)
colnames(newTitanic) <- c("Class","Survived","Sex","Age")
summary(newTitanic)
str(newTitanic)
newTitanic <- na.omit(newTitanic)
newTitanic <- as.data.frame(newTitanic)
newTitanic$Class <- as.factor(newTitanic$Class)
newTitanic$Survived <- as.factor(newTitanic$Survived)
newTitanic$Sex <- as.factor(newTitanic$Sex)
newTitanic$Age <- ifelse(newTitanic$Age < 18, "child", "adult")
newTitanic$Age <- as.factor(newTitanic$Age)

#Converting the data to binary variables

binaryData <- acm.disjonctif(newTitanic)

binData <- dist(binaryData, method = "binary")
eucidData <- dist(binaryData, method = "euclidean")

#Plotting based on binary distance
hClusBin <- hclust(binData, method = "ward.D2")
plot(hClusBin)

hClusteucid <- hclust(eucidData,method = "ward.D2")
plot(hClusteucid)

xMMBi <- cutree(hClusBin,3)
head(xMMBi)

y <- newTitanic[xMMBi == 1,]
summary(y)

y <- newTitanic[xMMBi == 2,]
summary(y)

y <- newTitanic[xMMBi == 3,]
summary(y)


xmmEu <- cutree(hClusteucid, 3)
y <- newTitanic[xmmEu == 1,]
summary(y)


