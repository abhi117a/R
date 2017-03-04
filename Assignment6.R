setwd("E:/Softwares/Rstudio")
popular <- read.csv("OnlineNewsPopularity.csv")
head(popular)
str(popular)
popular$url <- NULL
popular$timedelta <- NULL
ncol(popular)
lmR <- glm(formula = shares ~., data = popular, family = "gaussian")
summary(lmR)
#After first linear model summary we can see not all predictors are significant.
#We will use subset selection but we need to select predictors which are common
#There are way many predictors which will create computation problems when doing subset selection

library(leaps)

popularBest <- regsubsets(shares ~., popular, nvmax = 19, really.big = T)
