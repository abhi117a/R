consumer_survey <- read.csv("consumer-survey.csv")
head(consumer_survey)
myData <- consumer_survey
myData[is.na(myData)]=0
summary(myData)
ncol(myData)
myData$MILES <- NULL
myData$HYP_TYPE <- NULL
myData$TIME <- NULL
myData$Q_1B10 <- NULL
myData$Q_2BCL <- NULL
myData$Q_2BCNO <- NULL

summary(myData)
head(myData)

myData <- ifelse(myData == 1,TRUE, FALSE )

library(arules)
rules <- apriori(myData, parameter = list(supp = 0.5, conf = 0.85, target = "rules", minlen = 2))
inspect(rules)
rules <- apriori(myData, parameter = list(supp = 0.5, conf = 0.5, target = "rules", minlen = 2))

library(arulesViz)
plot(rules)
