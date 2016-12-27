HR <- read.csv("HR_comma_sep.csv", na.strings = "")
str(HR)
head(HR)
summary(HR)
#HR$left <- factor(HR$left)
#Plotting some Visualization
library(ggplot2)
g <- ggplot(HR)
g+ geom_boxplot(aes(x = HR$salary, y = HR$satisfaction_level, color = factor(HR$left)), na.rm = T)
g + geom_boxplot(aes(x = HR$average_montly_hours, y = HR$satisfaction_level, color = factor(HR$left)))
g + geom_boxplot(aes(x = HR$promotion_last_5years, y = HR$satisfaction_level, color = factor(HR$left)))

nrow(HR)

library(caTools)
split <- sample.split(HR$left, SplitRatio = 0.8)
train_HR <- subset(HR, split == T)
test_HR <- subset(HR,split == F)
nrow(test_HR)
nrow(train_HR)
x <- test_HR
x$left <- NULL

library(neuralnet)
nn <- neuralnet(left~satisfaction_level, data = train_HR, hidden = c(5), linear.output = FALSE)
plot(nn)

nn_predict <- compute(nn, x$satisfaction_level)
summary(nn_predict)
str(nn_predict)

pred_OUT <- nn_predict$net.result
observerd <- test_HR$left


MSE <- sum((observerd - pred_OUT)^2)/nrow(HR)
error.df <- data.frame(observerd,pred_OUT) 
head(error.df)


library(ggplot2)
ggplot(error.df,aes(x=observerd,y=pred_OUT)) + geom_point() + stat_smooth()
