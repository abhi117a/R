getwd()
vidGame <- read.csv("vgsales.csv")
head(vidGame)
vidGame$Platform
str(vidGame)
levels(vidGame$Platform)
vidGameClean<- vidGame[vidGame$Platform == "XOne" | vidGame$Platform == "PS4",]
head(vidGameClean)

library(ggplot2)
g <- ggplot(vidGameClean)
  geom_histogram(aes(x= vidGameClean$NA_Sales, fill = vidGameClean$Platform), color = "Black")
g

g+ geom_point(aes(x = vidGameClean$NA_Sales, y = vidGameClean$Publisher, color = vidGameClean$Platform))

head(vidGameClean)
str(vidGameClean)

library(caTools)
split <- sample.split(vidGameClean$Rank, SplitRatio =0.75 )
training_set <- subset(vidGameClean, split == TRUE)
nrow(training_set)
test_set <- subset(vidGameClean, split== FALSE)
nrow(test_set)


colnames(training_set)
training_set <- training_set[,-3:-5]
training_set <- training_set[,-1]
str(training_set)

training_set$Platform <- as.numeric(factor(training_set$Platform, 
                                              levels= c("XOne","PS4"), 
                                              labels = c(1,0)))

str(training_set)
library(xgboost)
classifier <- xgboost(data = as.matrix(training_set[,-1]), label = training_set$Platform, nrounds = 15)

#predicting
colnames(test_set)
str(test_set)
test_set$Platform

test_set <- test_set[,-2:-4]
test_set$Platform <- as.numeric(factor(test_set$Platform, 
                                           levels= c("XOne","PS4"), 
                                           labels = c(1,0)))
y_pred <- predict(classifier,newdata = as.matrix(test_set[,-1]))
y_pred <- ifelse(y_pred > 1 , 2 ,1)
cm <- table(test_set[,1],y_pred)

getwd()
t <- read.csv("Churn_Modelling.csv")
str(t)
str(vidGameClean)
