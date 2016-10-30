k <-(c("Germany","USA","USA","China","China"))
typeof(k)
k<- factor(k)
k <- factor(k,levels = c("Germany","USA","China"),labels = c(1,2,3))
str(k)
z <- as.numeric(as.character(z))
z
typeof(z)


PracData <- read.csv("DataUdemyML.csv", na.strings = "")
head(PracData,7)
tail(PracData,10)
#Removing <NA>
PracData[!complete.cases(PracData$Salary),]

medSalary<- median(PracData[,"Salary"], na.rm = T)
PracData[!complete.cases(PracData$Salary),"Salary"] <- medSalary

medAge <- median(PracData[,"Age"], na.rm = T)
PracData[is.na(PracData$Age),"Age"] <- medAge

#Converting Categorical data into factors

str(PracData)

#PracData$Country <- as.character(PracData$Country)
#PracData$Country <- factor(PracData$Country, levels = c("France","Spain","Germany"))
#PracData$Country <- factor(PracData$Country, labels = c("France","Spain","Germany"))



#PracData[is.na(PracData$Purchased),]

#PracData$Purchased <- as.character(PracData$Purchased)

#PracData$Purchased <- factor(PracData$Purchased, levels = c("No","Yes"))

#SplitData
#install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(PracData$Purchased, SplitRatio = 0.8)
split
training_set <- subset(PracData, split == T)
test_set <- subset(PracData, split == F)

#Feature Scaling [Can be done using Standarization or by Normalization]

training_set[,2:3] <- scale(training_set[,2:3])
test_set[,2:3] <- scale(test_set[,2:3])
