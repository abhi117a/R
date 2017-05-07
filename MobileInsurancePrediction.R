setwd("C:/Users/admin/Documents")

mobileInsurance <- read.csv("mobileInsuranceTrain.csv", sep = "\t")
ncol(mobileInsurance)
colnames(mobileInsurance) <- c("X0","X1","X2","X3","X4","X5","X6","X7","X8",
                               "X9","X10","X11","X12","X13","X14","X15","X16","X17",
                               "X18","X19","X20","X21","X22","X23","X24","X25","X26",
                               "X27","X28","X29","X30","X31","X32","X33","X34","X35",
                               "X36","X37","X38","X39","X40","X41","X42","X43","X44",
                               "X45","X46","X47","X48","X49","X50","X51","X52","X53",
                               "X54","X55","X56","X57","X58","X59","X60","X61","X62",
                               "X63","X64","X65","X66","X67","X68","X69","X70","X71",
                               "X72","X73","X74","X75","X76","X77","X78","X79","X80",
                               "X81","X82","X83","X84","Y")

str(mobileInsurance)



#Visualization

library(ggplot2)


ggplot(mobileInsurance) +
  geom_bar(aes(x= mobileInsurance$X0), fill = "red")

linModel <- lm(Y~., data= mobileInsurance)
plot(linModel)

summary(linModel)
#TrainTest
library(caTools)
split <- sample.split(mobileInsurance$Y, SplitRatio = 0.8)
mobileInsuranceTrain <- subset(mobileInsurance, split==T)
nrow(mobileInsuranceTrain)
mobileInsuranceTest <- subset(mobileInsurance, split==F)
nrow(mobileInsuranceTest)

#ML
#SVM 87% accuracy with kernel = radial

library(e1071)
SVMmodel <- svm(Y~., data = mobileInsuranceTrain)
summary(SVMmodel)
svmPred <- predict(SVMmodel, mobileInsuranceTest)

svmPred1 <- ifelse(svmPred>49,1,0)

table(mobileInsuranceTest$Y,svmPred1)

# perform a grid search
tuneResult <- tune(svm, Y ~ .,  data = mobileInsuranceTest,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph
plot(tuneResult)



#Categorical
#Naive bayes
#RandomFOrest - 93%
library(randomForest)

randomForest <- randomForest(Y~.,data = mobileInsuranceTrain)
RandomPred <- predict(randomForest, mobileInsuranceTest[-86])
table(mobileInsuranceTest$Y,RandomPred)


navie <- naiveBayes(Y~., data = mobileInsuranceTrain)
naivPred <- predict(navie, mobileInsuranceTest[-86])
table(mobileInsuranceTest$Y,naivPred)

#Gradient Boosting
library(gbm)
modelGB <- gbm(formula = Y ~ ., 
             distribution = "bernoulli",
             data = mobileInsuranceTrain,
             n.trees = 70,
             interaction.depth = 5,
             shrinkage = 0.3,
             bag.fraction = 0.5,
             train.fraction = 1.0,
             n.cores = NULL)

print(modelGB)
preds <- predict(modelGB, newdata = mobileInsuranceTest[-86], n.trees = 70)

labels <- mobileInsuranceTest$Y
cvAUC::AUC(predictions = preds, labels = labels)
#XGBoost
library(xgboost)
library(Matrix)

train.mx <- sparse.model.matrix(Y ~ ., mobileInsuranceTrain)
test.mx <- sparse.model.matrix(Y ~ ., mobileInsuranceTest)
dtrain <- xgb.DMatrix(train.mx, label = mobileInsuranceTrain[,"Y"])
dtest <- xgb.DMatrix(test.mx, label = mobileInsuranceTest[,"Y"])

train.gdbt <- xgb.train(params = list(objective = "binary:logistic",
                                      #num_class = 2,
                                      #eval_metric = "mlogloss",
                                      eta = 0.3,
                                      max_depth = 5,
                                      subsample = 1,
                                      colsample_bytree = 0.5), 
                        data = dtrain, 
                        nrounds = 70, 
                        watchlist = list(train = dtrain, test = dtest))

# Generate predictions on test dataset
preds <- predict(train.gdbt, newdata = dtest)
labels <- mobileInsuranceTest[,"Y"]

# Compute AUC on the test set
cvAUC::AUC(predictions = preds, labels = labels)
#XGBOOST -> 74.6 %
#Important features
library(Ckmeans.1d.dp)
names <- dimnames(data.matrix(mobileInsuranceTrain[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = train.gdbt)
xgb.plot.importance(importance_matrix[1:50,])
###


#Neural Net
library(neuralnet)

nn <- neuralnet(Y~X48+X60+X23+X2+X31,mobileInsuranceTrain,
                hidden=c(3,5),linear.output=FALSE)

#############################################################################
mobileInsurance$X0 <- as.factor(mobileInsurance$X0)
mobileInsurance$X1 <- as.factor(mobileInsurance$X1)
mobileInsurance$X2 <- as.factor(mobileInsurance$X2)
mobileInsurance$X3 <- as.factor(mobileInsurance$X3)
mobileInsurance$X4 <- as.factor(mobileInsurance$X4)
mobileInsurance$X5 <- as.factor(mobileInsurance$X5)
mobileInsurance$X6 <- as.factor(mobileInsurance$X6)
mobileInsurance$X7 <- as.factor(mobileInsurance$X7)
mobileInsurance$X9 <- as.factor(mobileInsurance$X8)
mobileInsurance$X10 <- as.factor(mobileInsurance$X10)
mobileInsurance$X11 <- as.factor(mobileInsurance$X11)
mobileInsurance$X12 <- as.factor(mobileInsurance$X12)
mobileInsurance$X13 <- as.factor(mobileInsurance$X13)
mobileInsurance$X14 <- as.factor(mobileInsurance$X15)
mobileInsurance$X16 <- as.factor(mobileInsurance$X16)
mobileInsurance$X17 <- as.factor(mobileInsurance$X17)
mobileInsurance$X18 <- as.factor(mobileInsurance$X18)
mobileInsurance$X19 <- as.factor(mobileInsurance$X19)
mobileInsurance$X20 <- as.factor(mobileInsurance$X20)
mobileInsurance$X21 <- as.factor(mobileInsurance$X21)
mobileInsurance$X22 <- as.factor(mobileInsurance$X22)
mobileInsurance$X23 <- as.factor(mobileInsurance$X23)
mobileInsurance$X24 <- as.factor(mobileInsurance$X24)
mobileInsurance$X25 <- as.factor(mobileInsurance$X25)
mobileInsurance$X26 <- as.factor(mobileInsurance$X26)
mobileInsurance$X27 <- as.factor(mobileInsurance$X27)
mobileInsurance$X28 <- as.factor(mobileInsurance$X28)
mobileInsurance$X29 <- as.factor(mobileInsurance$X29)
mobileInsurance$X30 <- as.factor(mobileInsurance$X30)
mobileInsurance$X31 <- as.factor(mobileInsurance$X31)
mobileInsurance$X32 <- as.factor(mobileInsurance$X32)
mobileInsurance$X33 <- as.factor(mobileInsurance$X33)
mobileInsurance$X34 <- as.factor(mobileInsurance$X34)
mobileInsurance$X35 <- as.factor(mobileInsurance$X35)
mobileInsurance$X36 <- as.factor(mobileInsurance$X36)
mobileInsurance$X37 <- as.factor(mobileInsurance$X37)
mobileInsurance$X38 <- as.factor(mobileInsurance$X38)
mobileInsurance$X39 <- as.factor(mobileInsurance$X39)
mobileInsurance$X40 <- as.factor(mobileInsurance$X40)
mobileInsurance$X41 <- as.factor(mobileInsurance$X41)
mobileInsurance$X42 <- as.factor(mobileInsurance$X42)
mobileInsurance$X43 <- as.factor(mobileInsurance$X43)
mobileInsurance$X44<- as.factor(mobileInsurance$X44)
mobileInsurance$X45 <- as.factor(mobileInsurance$X45)
mobileInsurance$X46 <- as.factor(mobileInsurance$X46)
mobileInsurance$X47 <- as.factor(mobileInsurance$X47)
mobileInsurance$X48 <- as.factor(mobileInsurance$X48)
mobileInsurance$X49 <- as.factor(mobileInsurance$X49)
mobileInsurance$X50 <- as.factor(mobileInsurance$X50)
mobileInsurance$X51 <- as.factor(mobileInsurance$X51)
mobileInsurance$X52 <- as.factor(mobileInsurance$X52)

mobileInsurance$X53 <- as.factor(mobileInsurance$X53)
mobileInsurance$X54 <- as.factor(mobileInsurance$X54)
mobileInsurance$X55 <- as.factor(mobileInsurance$X55)
mobileInsurance$X56 <- as.factor(mobileInsurance$X56)
mobileInsurance$X57 <- as.factor(mobileInsurance$X57)
mobileInsurance$X58 <- as.factor(mobileInsurance$X58)
mobileInsurance$X59 <- as.factor(mobileInsurance$X59)
mobileInsurance$X60 <- as.factor(mobileInsurance$X60)
mobileInsurance$X61 <- as.factor(mobileInsurance$X61)
mobileInsurance$X62 <- as.factor(mobileInsurance$X62)
mobileInsurance$X63 <- as.factor(mobileInsurance$X63)
mobileInsurance$X64 <- as.factor(mobileInsurance$X64)
mobileInsurance$X65 <- as.factor(mobileInsurance$X65)
mobileInsurance$X66 <- as.factor(mobileInsurance$X66)
mobileInsurance$X67 <- as.factor(mobileInsurance$X67)
mobileInsurance$X68 <- as.factor(mobileInsurance$X68)
mobileInsurance$X69 <- as.factor(mobileInsurance$X69)
mobileInsurance$X70 <- as.factor(mobileInsurance$X70)
mobileInsurance$X71 <- as.factor(mobileInsurance$X71)
mobileInsurance$X72 <- as.factor(mobileInsurance$X72)
mobileInsurance$X73 <- as.factor(mobileInsurance$X73)
mobileInsurance$X74 <- as.factor(mobileInsurance$X74)
mobileInsurance$X75 <- as.factor(mobileInsurance$X75)
mobileInsurance$X76 <- as.factor(mobileInsurance$X76)
mobileInsurance$X77 <- as.factor(mobileInsurance$X77)
mobileInsurance$X78 <- as.factor(mobileInsurance$X78)
mobileInsurance$X79 <- as.factor(mobileInsurance$X79)
mobileInsurance$X80 <- as.factor(mobileInsurance$X80)
mobileInsurance$X81 <- as.factor(mobileInsurance$X81)
mobileInsurance$X81 <- as.factor(mobileInsurance$X82)
mobileInsurance$X83 <- as.factor(mobileInsurance$X83)
mobileInsurance$X84 <- as.factor(mobileInsurance$X84)
mobileInsurance$Y <- as.factor(mobileInsurance$Y)