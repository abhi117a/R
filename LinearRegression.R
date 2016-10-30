getwd()
data_salary<- read.csv("Salary_Data.CSV",na.strings = "")
head(data_salary)

data_salary[is.na(data_salary),]
data_salary[!complete.cases(data_salary),]

library(caTools)
split <- sample.split(data_salary$Salary, SplitRatio = 0.8)
traingSet <- subset(data_salary, split==T)
testSet <- subset(data_salary, split==F)

regressor = lm(formula = Salary ~ YearsExperience,data =traingSet)
summary(regressor)
