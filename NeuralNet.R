library(MASS)
df <- Boston

str(df)
summary(df)

library(ggplot2)
ggplot(df)+
  geom_histogram(aes(x=df$age), color = "black")

  ggplot(df)+
    geom_histogram(aes(x=df$crim, fill = df$age), color = "black") + scale_color_gradient(high = "red", low = "blue")
  
  
  library(neuralnet)
  maxs <- apply(df, 2,max)
  mins <- apply(df, 2,min)
  
  scaled <- as.data.frame(scale(df,center = mins,scale = maxs-mins))
  
  
  #Splitting Data
  
  library(caTools)
  split <- sample.split(scaled$medv, SplitRatio = 0.75)
  train_df <- subset(scaled, split==T)
  test_df <- subset(scaled, split==F)
  
  nn <- neuralnet(formula = medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
                    tax + ptratio + black + lstat, data = train_df, hidden = c(5,3), linear.output = T)
  
  plot(nn)
  
  #predict the values
  
pred <- compute(nn, test_df[,1:13])

summary(pred)
str(pred)

true_predictions <- pred$net.result*(max(df$medv)-min(df$medv))+min(df$medv)
test_r <- (test_df$medv)*(max(df$medv)-min(df$medv))+min(df$medv)

MSE_nn <- sum((test_r - true_predictions)^2)/nrow(test_df)

error.df <- data.frame(test_r,true_predictions)


error_df <- data.frame(test_df$medv,pred$net.result)

library(ggplot2)
ggplot(error_df,aes(x=test_r,y=true_predictions)) + geom_point() + stat_smooth()