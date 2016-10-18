set.seed(1234567890)
#mydata <- read.table("E:/UtDallas/ML/Assignments/Assignment-2/data.csv",sep=",", header=TRUE, row.names="ID")
mydata <- read.csv("data.csv")
# to check if data contains NA
any(is.na(mydata))

head(mydata)
dim(mydata)
summary(mydata)


library("corrplot")
M <- cor(mydata)
corrplot(M,method = "number")

#--------------Histogram & Bar Charts--------------#

library(ggplot2)

g <- ggplot(data = mydata,aes(x=mydata$LIMIT_BAL))
g+ geom_histogram(binwidth = 3000) +xlab("Balance Limit") + ylab("Frequency")

g <- ggplot(data = mydata,aes(x=mydata$AGE))
g+ geom_histogram(binwidth = 5,color="white") +xlab("Age of Customer") + ylab("Frequency")


g <- ggplot(data = mydata,aes(x=mydata$EDUCATION))
g+ geom_histogram(binwidth = 0.1 ,color="white") +xlab("Education") + ylab("Frequency")


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT1,y=mydata$PAY_AMT1,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_0)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt1") + ylab("Pay_AMt1")+ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT2,y=mydata$PAY_AMT2,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_2)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt2") + ylab("Pay_AMt2")+ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT3,y=mydata$PAY_AMT3,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_3)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt3") + ylab("Pay_AMt3")+ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT4,y=mydata$PAY_AMT4,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_4)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt4") + ylab("Pay_AMt4")+ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT5,y=mydata$PAY_AMT5,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_5)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt5") + ylab("Pay_AMt5")+ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))


k <- ggplot(data = mydata,aes(x=mydata$BILL_AMT6,y=mydata$PAY_AMT6,color=mydata$IS_DEFAULT))
k+ geom_point(aes(size=mydata$PAY_6)) +xlim(1000,1500)+ylim(1000,1500) + scale_colour_gradientn(colours=rainbow(4)) +
  xlab("Bill_Amnt6") + ylab("Pay_AMt6") +ggtitle("Bill_Amnt VS PAy_Amnt") +theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.x = element_text(size=10),axis.text.y = element_text(size=10),
                                                                                 legend.title = element_text(size = 20),legend.text = element_text(size = 10), plot.title= element_text(size = 25))



##############################DECISION TREE########################################

library(rpart)
library(rpart.plot)
library(pROC)

### Train and Test data sample ####
trainIndex <- sample(1:nrow(mydata), 0.8 * nrow(mydata))
train <- mydata[trainIndex, ]
test <- mydata[-trainIndex, ]

#### Decision tree ####
fit <- rpart(IS_DEFAULT ~ .,method="class", data=train)
rpart.plot(fit)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 

#### predict ####
p <- predict(fit, test,type = 'class')
table(test[,24], p)

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1


####### with minsplit = 15
fit <- rpart(IS_DEFAULT ~ .,method="class", data=train, control = rpart.control(minsplit = 15, cp = 1e-04))
p <- predict(fit, test,type = 'class')
table(test[,24], p)

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

####### with minsplit = 5 ########
fit <- rpart(IS_DEFAULT ~ .,method="class", data=train, control = rpart.control(minsplit = 5, cp = 1e-03))


p <- predict(fit, test,type = 'class')
head(table(test[,24], p))

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1


####### with minsplit = 10 ########
fit <- rpart(IS_DEFAULT ~ .,method="class", data=train, control = rpart.control(minsplit = 10, cp = 1e-02))
p <- predict(fit, test,type = 'class')
head(table(test[,24], p))

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

####### with minsplit = 7 ########
fit <- rpart(IS_DEFAULT ~ .,method="class", data=train, control = rpart.control(minsplit = 7, cp = 1e-05))
p <- predict(fit, test,type = 'class')
head(table(test[,24], p))

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

####### with minsplit = 3 ########
fit <- rpart(IS_DEFAULT ~ PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 , parms = list(split = 'information'), control = rpart.control(minsplit = 5, cp = 1e-04),  method="class", data=train)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
library(rattle)
fancyRpartPlot(fit)

rpart.plot(fit)
p <- predict(fit, test,type = 'class')
head(table(test[,24], p))

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

################# PRUNING ###############


?printcp
?rpart
pfit<- prune(fit, cp=-0.002)
printcp(pfit) # display the results 
plotcp(pfit) # visualize cross-validation results 


p <- predict(pfit, test,type = 'class')
head(table(test[,24], p))

#### compute ROC curve ####
ROC1 <- roc(test[,24], as.numeric(p))
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1



############################################################ PERCEPTRON ####################################

library("neuralnet")

fit <- neuralnet(IS_DEFAULT ~ PAY_0 + PAY_2  , train, hidden = 0, lifesign = "none",linear.output = FALSE, likelihood=FALSE)
plot(fit, rep = "best")

temp_test <- subset(test, select = c("PAY_0","PAY_2"))
fit.results <- compute(fit, temp_test)

results <- data.frame(actual = test[,24], prediction = fit.results$net.result)
results$prediction <- round(results$prediction)

head(table(results$actual, results$prediction ))

#### compute ROC curve ####
ROC1 <- roc(results$actual, results$prediction )
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

##########################################################Neural NET #######################################

fit <- neuralnet(IS_DEFAULT ~ PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 , train, hidden = 5, lifesign = "minimal",linear.output = FALSE, likelihood=FALSE, threshold = 0.1)
plot(fit, rep = "best")
temp_test <- subset(test, select = c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6"))
fit.results <- compute(fit, temp_test)

results <- data.frame(actual = test[,24], prediction = fit.results$net.result)
results$prediction <- round(results$prediction)

head(table(results$actual, results$prediction ))

#### compute ROC curve ####
ROC1 <- roc(results$actual, results$prediction )
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1

#######################################################SVM #################################################

library("e1071")
svm_model <- svm(IS_DEFAULT ~ PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6, data=train,  kernel="radial", cost=1, gamma=0.5)
summary(svm_model)
x <- subset(test, select=-IS_DEFAULT)
pred <- predict(svm_model,x)
y <- test[,24]
head(table(round(pred),y))
#### compute ROC curve ####
ROC1 <- roc( y,round(pred) )
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1


#################################################### Naive Bayes ##########################################
str(mydata)
library("e1071")
naiveBayes <- naiveBayes(IS_DEFAULT ~ EDUCATION +MARRIAGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 ,train)
naiveBayes
summary(naiveBayes)
p <- predict(naiveBayes, test[,-24], type = c("raw"))
p <- round(p)
p <- p[,2]
y <- test[,24]
head(table(p,y))
#### compute ROC curve ####
ROC1 <- roc( y,p )
plot(ROC1, col = "blue")
AUC1 <- auc(ROC1)
AUC1
