#Liner Regression on Kaggle DataSet

stud_mat <- read.csv("student-mat.csv", sep = ";")
head(stud_mat)
summary(stud_mat)
str(stud_mat)
#Check if dataset has null value
any(is.na(stud_mat))
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
num_cols <- sapply(stud_mat,is.numeric)
cor_data <- cor(stud_mat[,num_cols])
#Correlation Plot
corrplot(cor_data, method = "color")
corrgram(stud_mat)
corrgram(stud_mat, order = T, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
ggplot(stud_mat, aes(x = stud_mat$G3)) + geom_histogram(color = "blue", fill = "red")
#Splitting Data
set.seed(101)
library(caTools)
split <- sample.split(stud_mat$G3, SplitRatio = 0.7)
train_Stud <- subset(stud_mat, split == T)
test_stud <- subset(stud_mat, split==F)


model <- lm(G3 ~., data = stud_mat)
summary(model)

