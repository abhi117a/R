#Polynomial Regression
sal_data <- read.csv("Position_Salaries.csv")
head(sal_data)
sal_data <- sal_data[,2:3]

sal_data$Level2 <- sal_data$Level^2
sal_data$Level3 <- sal_data$Level^3
poly_reg <- lm(formula = Salary ~.,data = sal_data)
summary(poly_reg)
