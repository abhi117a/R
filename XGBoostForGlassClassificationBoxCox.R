glass <- read.csv("glass.csv")
head(glass)
str(glass)

library(caret)
library(caTools)

split <- sample.split(glass$Type, SplitRatio = 0.85)
glass_train <- subset(glass,split == TRUE)
glass_test <- subset(glass,split == FALSE)
nrow(glass_train)
nrow(glass_test)
ncol(glass_test)
ncol(glass_train)
colnames(glass_train)
#XGBoost with BoxCox Transformation

classifierXGBoost <- train(Type~.,
                           data = glass_train,
                           method = "xgbTree",
                           preProcess = "BoxCox",
                          trainContrl = trainControl(method = "repeatedcv",
                                                     number = 5, repeats = 10,
                                                     verboseIter = FALSE))



y <- predict(classifierXGBoost, glass_test[,-10])
cm <- table(y,glass_test[,10])

confusionMatrix(y,glass_test[,10])
