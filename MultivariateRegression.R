startUps <- read.csv("50_Startups.csv", na.strings = "")
head(startUps)
summary(startUps)
str(startUps)

startUps$State = factor(startUps$State, levels = c('California','Florida','New York'), labels = c(2,3,1))

library(caTools)
set.seed(12345)

split = sample.split(Y = startUps$Profit, SplitRatio = 0.8)

traing_set = subset(startUps, split == T)
test_set = subset(startUps, split == F)
#traing_set
#test_set
regressor = lm(formula = Profit ~.,data=traing_set)

summary(regressor)
#Predicitng 

y_pred = predict(regressor,newdata = test_set)
y_pred

library(ggplot2)
g <- ggplot(data= test_set)
g+geom_point(aes(x=test_set$R.D.Spend,y=test_set$Profit), color = "Blue") +
  geom_line(aes(x=test_set$R.D.Spend,y=y_pred),color = "Red")


