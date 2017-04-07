diaBetes <- read.csv("diabetestData.csv")
head(diaBetes)
str(diaBetes)

#diaBetes[,1] <- ifelse(diaBetes[,1] == 0, NA,diaBetes[,1])
tmp = ncol(diaBetes) -1
tmp

for(i in 1:tmp){
  diaBetes[,i] <- ifelse(diaBetes[,i] == 0, NA,diaBetes[,i])
  print(i)
}

diaBetes<- na.omit(diaBetes)
diaBetes[,1]


str(diaBetes)
#diaBetes$Class <- as.factor(diaBetes$Class)

####

library(caTools)
split = sample.split(diaBetes$Class, SplitRatio = 0.8)
trainDia <- subset(diaBetes, split==TRUE)
nrow(trainDia)
testDia <- subset(diaBetes, split==FALSE)
nrow(testDia)

library(neuralnet)
nn = neuralnet(Class~NoTimesPreg+PlasmaGlucose+Diastolic+Triceps+serumInsulin+
                 BMI+DiabetesPedigree+Age, data = trainDia, hidden = c(5,6), linear.output = FALSE, threshold = 1)

plot(nn)

predict1 <- compute(nn,testDia[-9])
val <- ifelse(predict1$net.result >= 0.49,1,0)

val
table(testDia[,9],val)
