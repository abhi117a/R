setwd("C:/Users/admin/Documents")
auto <- read.csv("AutoMPG.csv", na.strings  = "?")
head(auto)
str(auto)
summary(auto)
auto$hp <- as.numeric(auto$hp)
auto[is.na(auto)]
auto$model
#Visualization
qqnorm(auto$mpg)
qqline(auto$mpg)
#Heavy Tailed
library(ggplot2)
g <- ggplot(auto)
g+ geom_point(aes(y=auto$mpg,x=auto$displacement, 
                  color= factor(auto$model), 
                  size = auto$cylinders))

cor(auto[,-9])
library(corrgram)
corrgram(auto[,-9], lower.panel = panel.pie, upper.panel = NULL)

lmR <- lm(formula = mpg ~.-carName, data = auto)
summary(lmR)

plot(lmR$residuals,auto[,1]) 
#Plot seems fairly random no pattern can be seen so we can assume safely that no polynomial term is necessary

#Updating model based on summary and plots
lmR <- lm(formula = mpg ~ weight + model + origin, data = auto)
summary(lmR)

