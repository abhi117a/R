#Random Forest Regression
data_forest <- read.csv("Position_Salaries.csv")
data_forest <- data_forest[,2:3]

#install.packages("randomForest")

typeof(data_forest)
typeof(data_forest$Level)
typeof(data_forest[1])
typeof(data_forest[,1])


library(randomForest)
set.seed(1234)
#for randomFOrest() x takes dataFrame while y takes vector that's why we are using different style of data input
regressor_forest <- randomForest(x = data_forest[1], y= data_forest$Salary, ntree = 100 )

y_predict <- predict(regressor_forest, data.frame(Level = 6.5))


library(ggplot2)
x_grid <- seq(min(data_forest$Level), max(data_forest$Level), 0.01)
ggplot()+
  geom_point(aes(x=data_forest$Level, y=data_forest$Salary), color = "Red") +
  geom_line(aes(x=x_grid, y= predict(regressor_forest,newdata = data.frame(Level = x_grid))),color = 'blue')