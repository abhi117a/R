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


traing_set
test_set
