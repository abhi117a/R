
con <- file("assignment-4.log")

sink(con, append=TRUE)
sink(con, append=TRUE, type="message")


library(caret)
library(mlbench)
library(e1071)
set.seed(1234567890)
mydata <- read.table("data.csv",sep=",", header=TRUE, row.names="ID")

# to check if data contains NA
any(is.na(mydata))

head(mydata)
dim(mydata)
summary(mydata)

trainIndex <- sample(1:nrow(mydata), 0.8 * nrow(mydata))
train <- mydata[trainIndex, ]
test <- mydata[-trainIndex, ]

Y <- factor(ifelse(train$IS_DEFAULT=='1', "yes", "no"))

############################################# MODELS ####################################

############ BOOSTING ###############################################

control <- trainControl(method="repeatedcv", number=10, classProbs = TRUE,verboseIter = TRUE)
fit <- train(train[,1:23],Y, method="gbm", trControl=control, metric = "Accuracy" ,preProcess = c("center","scale"),tuneLength = 2)
print(fit)

##################################### RANDOM FOREST ###############################################
control <- trainControl(method="repeatedcv", number=10, classProbs = TRUE,verboseIter = TRUE)
fit <- train(train[,1:23],Y, method="rf", trControl=control, metric = "Accuracy" ,preProcess = c("center","scale"),tuneLength = 2)
print(fit)

##################################### BAGGING ###############################################
control <- trainControl(method="repeatedcv", number=10, classProbs = TRUE,verboseIter = TRUE)
fit_bag <- train(train[,1:23],Y, method="treebag", trControl=control, metric = "Accuracy" ,preProcess = c("center","scale"),tuneLength = 10)
print(fit_bag)

##################################### KNN ###############################################
control <- trainControl(method="repeatedcv", number=3, classProbs = TRUE,verboseIter = TRUE)
fit <- train(train[,1:23],Y, method="knn", trControl=control, metric = "Accuracy" ,preProcess = c("center","scale"),tuneLength = 20)
print(fit)

##################################### Logistic Regression ###############################################
objControl <- trainControl(method='cv', number=10, returnResamp='none',verboseIter = TRUE, classProbs = TRUE)
fit <- train(train[,6:23],Y, method="glm", trControl=objControl, metric = "Accuracy" )
# display results
print(fit)


################### ROC ##################
plot(fit)
p <- predict(fit, test)
pred <- factor(ifelse(train$IS_DEFAULT=='1', '1', '0'))
head(unclass(pred))
p <- round(p)
p <- p[,2]
y <- test[,24]
table(p,y)
#########################
