HR <- read.csv("HR_comma_sep.csv", na.strings = "")
str(HR)
head(HR)
summary(HR)
#Plotting some Visualization
library(ggplot2)
g <- ggplot(HR)
g+ geom_boxplot(aes(x = HR$salary, y = HR$satisfaction_level, color = factor(HR$left)), na.rm = T)
g + geom_boxplot(aes(x = HR$average_montly_hours, y = HR$satisfaction_level, color = factor(HR$left)))
g + geom_boxplot(aes(x = HR$promotion_last_5years, y = HR$satisfaction_level, color = factor(HR$left)))

boxplot(x=HR[,8])
