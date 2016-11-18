#Logistic Regression
df_train <- read.csv("titanic_train.csv")
head(df_train)
str(df_train)
#CHecking Missing Data
library(Amelia)
missmap(df_train,main = "Missing Map", col = c("yellow","black"), legend = F)

#Exploring Data

library(ggplot2)
ggplot(df_train) +
  geom_histogram(aes(x=df_train$Age, fill = df_train$Sex), color = "black") + theme_bw()


ggplot(df_train, aes(x=factor(df_train$Pclass), y=df_train$Age, color = df_train$Sex)) + geom_jitter()+
  geom_boxplot(alpha = 0.7, size = 1.2) 

ggplot(df_train) + geom_histogram(aes(x=df_train$Fare, fill = df_train$Sex), color = "black")


# Replacing missing Data with Mean values

head(df_train[is.na(df_train[,"Age"]),])

mean1 <- mean(df_train[df_train$Pclass==1,"Age"], na.rm = T)
mean2 <- mean(df_train[df_train$Pclass==2,"Age"], na.rm = T)
mean3<- mean(df_train[df_train$Pclass==3,"Age"], na.rm = T)

df_train[df_train$Pclass==1 & is.na(df_train$Age),"Age"] <- mean1
df_train[df_train$Pclass==2 & is.na(df_train$Age),"Age"] <- mean2
df_train[df_train$Pclass==3,"Age"]
df_train[df_train$Pclass==3 & is.na(df_train$Age),"Age"] <- mean3

library(dplyr)
df_train <- select(df_train,-PassengerId,-Name,-Ticket,-Cabin)
df_train$Survived <- factor(df_train$Survived)
df_train$Pclass <- factor(df_train$Pclass)
df_train$Parch <- factor(df_train$Parch)
df_train$SibSp <- factor(df_train$SibSp)

str(df_train)


#Modeling

logModel <- glm(Survived~.,family = binomial(link="logit"), data = df_train)
summary(logModel)

#Practicing of Split

library(caTools)
split <- sample.split(df_train$Survived, SplitRatio = 0.75)
train <- subset(df_train, split== T)
test <- subset(df_train, split == F)


lofFinalModel <- glm(Survived~., family = binomial(link = "logit"), data = train)
summary(lofFinalModel)

predictTest <- predict(lofFinalModel,newdata = test, type = "response")

result <- ifelse(predictTest > 0.5, 1,0)

misClasificError <- mean(result!=test$Survived)
print("Accuracy")
1-misClasificError


table(test$Survived, predictTest > 0.5)
#############################################################################################

titan_test <- read.csv("titanic_test.csv")
titan_test[is.na(titan_test)]
missmap(titan_test,col = c("yellow", "black"))


head(titan_test)

titan_test[titan_test$Pclass==1 & is.na(titan_test$Age),]
med1 <- median(titan_test[titan_test$Pclass==1,"Age"], na.rm = T)
titan_test[titan_test$Pclass==1 & is.na(titan_test$Age),"Age"] <- med1


titan_test[titan_test$Pclass==2 & is.na(titan_test$Age),]
med2 <- median(titan_test[titan_test$Pclass==2,"Age"], na.rm = T)
titan_test[titan_test$Pclass==2 & is.na(titan_test$Age),"Age"] <- med2


titan_test[titan_test$Pclass==3 & is.na(titan_test$Age),]
med3 <- median(titan_test[titan_test$Pclass==3,"Age"], na.rm = T)
titan_test[titan_test$Pclass==3 & is.na(titan_test$Age),"Age"] <- med3


titan_test[is.na(titan_test$Fare),] <- NULL
