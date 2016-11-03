#SVR(Support Vector Regression)
SVR_data <- read.csv("Position_Salaries_SVR.csv",na.strings = "")
head(SVR_data)

SVR_data <- SVR_data[,2:3]
library(e1071)
regressor_SVR = svm(formula = Salary~., data = SVR_data, type = "eps-regression")
y_pred <- predict(regressor_SVR,data.frame(Level = 6.5))
y_pred
