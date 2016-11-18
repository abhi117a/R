#Logistic Regression
df_train <- read.csv("titanic_train.csv")
head(df_train)
str(df_train)
library(Amelia)
missmap(df_train,main = "Missing Map", col = c("yellow","black"), legend = F)

library(ggplot2)
ggplot(df_train) +
  geom_histogram(aes(x=df_train$Age, fill = df_train$Sex), color = "black") + theme_bw()


ggplot(df_train, aes(x=factor(df_train$Pclass), y=df_train$Age, color = df_train$Sex)) + geom_jitter()+
  geom_boxplot(alpha = 0.7, size = 1.2) 

ggplot(df_train) + geom_histogram(aes(x=df_train$Fare, fill = df_train$Sex), color = "black")


head(df_train[is.na(df_train[,"Age"]),])

mean1 <- mean(df_train[df_train$Pclass==1,"Age"], na.rm = T)
mean2 <- mean(df_train[df_train$Pclass==2,"Age"], na.rm = T)
mean3<- mean(df_train[df_train$Pclass==3,"Age"], na.rm = T)

df_train[df_train$Pclass==1 & is.na(df_train$Age),"Age"] <- mean1
df_train[df_train$Pclass==2 & is.na(df_train$Age),"Age"] <- mean2
df_train[df_train$Pclass==3,"Age"]
df_train[df_train$Pclass==3 & is.na(df_train$Age),"Age"] <- mean3
