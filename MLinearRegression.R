setwd("C:/Users/admin/Documents")
spirit <- read.csv("spiritwt_data.csv", header = FALSE)
head(spirit)
colnames(spirit) = c("Temp","Dilution","Wght")
summary(spirit)
# We'll build a simple linear regression model first
lmR <- lm(formula = Wght~., data = spirit)
summary(lmR)
# Much improved over single variable (R^2)
plot(lmR$fitted.values,lmR$residuals)
plot(spirit$Temp,lmR$residuals)
plot(spirit$Dilution,lmR$residuals)
# Looks like there is nonlinearity in the dilution variable
lmR2 <- lm(formula = Wght~Temp+poly(Dilution,2), data = spirit)
summary(lmR2)
# All p-values small, good sign ...
plot(spirit$Dilution,lmR2$residuals)
# Still non-linear effect ...
lmR4 <- lm(formula = Wght ~ Temp+poly(Dilution,4), data = spirit)
summary(lmR4)
# It may be that the variance is not constant ...
plot(spirit$Dilution, lmR4$residuals)
qqnorm(lmR4$residuals)
qqline(lmR4$residuals)

# Another way to introduce nonlinearity (gently) is with a
# cross term.
lmrCross <- lm(formula = Wght~Temp+Dilution+Temp*Dilution, data = spirit) 
summary(lmrCross)
plot(spirit$Dilution, lmrCross$residuals)
# The p-vlaue is too high, this term is probably not significant
# There is probably no interaction between these two predictors