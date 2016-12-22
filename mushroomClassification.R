#MushRoom Classification
mushroom <- read.csv("mushrooms.csv")
head(mushroom,1)
str(mushroom)

library(caTools)
split <- sample.split(mushroom$class, SplitRatio = 0.7)
mushroom_train <- subset(mushroom, split == TRUE)
mushroom_test <- subset(mushroom, split == FALSE)
nrow(mushroom_train)
nrow(mushroom_test)
x <- mushroom_test
x$class <- NULL

library(rpart)
regressor = rpart(formula = class~., data = mushroom_train, control = rpart.control(minsplit = 1))

y_pred = predict(regressor, x)
head(y_pred)
