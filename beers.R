setwd("C:/Users/admin/Documents")
beers <- read.csv("beers.csv")
head(beers)
beers$X <- NULL
summary(beers)
nrow(beers)
#Replacing NA values of Alcohol content with median values of the column
meadianABV <- median(beers$abv, na.rm = T)
beers[is.na(beers$abv), "abv"] <- meadianABV
#Replacing NA values of Bitterness with median values of the column
medianIbu <- median(beers$ibu, na.rm = T)
beers[is.na(beers$ibu),"ibu"] <- medianIbu


library(ggplot2)
b <- ggplot(beers)
b+ geom_point(aes(x = beers$abv,y=beers$ibu, size = beers$ounces, 
                  color = as.factor(beers$ounces)), alpha = 0.7) +
              ggtitle("Alcohol Content vs Bitterness") + xlab("Alcohol Content")+ ylab( "Bitterness")
#We can see a positive corelation between bitterness and alcohol content.
#Let's perform linear regression on this as we can see only 2 variables are the most significant

#dividing data into test and train
library(caTools)
split <- sample.split(beers$abv, SplitRatio = 0.8)
beersTrain <- subset(beers, split == TRUE)
nrow(beers)
beersTest <- subset(beers, split == FALSE)
nrow(beersTest)
lmBeers <- glm(formula = abv~ibu, data = beersTrain, family = "gaussian")
summary(lmBeers)
nrow(beers)
MSE <- mean(lmBeers$residuals^2)

predY <- predict(lmBeers, newdata = beersTest[,-1])
summary(predY)

mean((predY-beersTest[,1])^2)

#Visualizing the results

g <- ggplot()
g + geom_point(aes(x= beersTest$ibu, y = beersTest$abv),
               color = "blue") +
  geom_line(aes(x = beersTest$ibu, y = predY), color = "red") +
  ggtitle("Alcohol Content vs Bitterness of Test set") + xlab("Alcohol Content")+ ylab( "Bitterness")

#Using Cross Validation Technique
library(boot)
cv.error = rep (0 ,10)
  for (i in 1:10) {
   glm.fit=glm(abv~ibu,data=beers)
  cv.error[i]=cv.glm(beers ,glm.fit ,K=10) $delta [1] }

cv.error
plot(cv.error, main = "Cross Validation")
