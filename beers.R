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

temp <- as.data.frame(cbind(beers$abv,beers$ibu))
head(temp)
head(temp)
library(corrgram)
corrgram(temp)

library(ggplot2)
b <- ggplot(beers)
b+ geom_point(aes(x = beers$abv,y=beers$ibu, size = beers$ounces, 
                  color = as.factor(beers$ounces)), alpha = 0.7)
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

predY <- predict(lmBeers, newdata = beersTest[,-1])
summary(predY)


g <- ggplot()
g + geom_point(aes(x= beersTest$ibu, y = beersTest$abv),
               color = "blue") +
  geom_line(aes(x = beersTest$ibu, y = predY), color = "red")

cm <- table(predY,beersTest[,1])

plot(predY,beersTest[,1])
