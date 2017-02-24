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
plot(lmR$residuals,lmR$fitted.values)
plot(auto$weight,lmR$residuals)
plot(auto$model,lmR$residuals)
plot(auto$origin,lmR$residuals)
lmR <- glm(formula = mpg ~ weight + model + origin, data = auto)
# No non-linear effects
#After fitting the model let's check the accuracy using the CV techniques
#LOOCV approch
library(boot)
cv_error <- cv.glm(auto[,-9],lmR)
cv_error$delta
#K-Fold using K=10
cv_errK <- cv.glm(auto[,-9],lmR,K = 10)
cv_errK$delta

student_por <- read.csv("student-por.csv",sep = ";")
head(student_por)
summary(student_por)
LinModel <- glm(formula = G1~.,data = student_por,family = gaussian)
summary(LinModel)
plot(LinModel$residuals,LinModel$fitted.values)
plot(student_por$G2,LinModel$residuals)
plot(student_por$G3,LinModel$residuals)
plot(student_por$school,LinModel$residuals)

#After initial model we see there are lot parameters which are not significant
#try removing them and creating new model, also saw few residual plots which seems like there is non linear relationship
#we will try it
#Linear Model with G2 and G3 including
summary(student_por)
str(student_por)
student_por$G2
LinModel1 <- glm(formula = G1~school+age+schoolsup+absences+G2+G3,
                 data = student_por,family = gaussian)
summary(LinModel1)
LinModel1LoocV <- cv.glm(student_por,LinModel1)

sqrt(mean(LinModel1$residuals^2))

LinModel1LoocV$delta


