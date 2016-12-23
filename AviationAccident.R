accident <- read.csv("AviationDataUP.csv")
head(accident)
str(accident)
accident$Event.Id <- NULL
accident$Latitude <- NULL
accident$Longitude <- NULL
accident$Total.Fatal.Injuries <- NULL
accident$Total.Serious.Injuries <- NULL
accident$Total.Minor.Injuries <- NULL
accident$Total.Uninjured <- NULL
accident$Registration.Number <- NULL
accident$Accident.Number <- NULL

library(caTools)
split <- sample.split(accident$Aircraft.Damage, SplitRatio = 0.8)
accident_train <- subset(accident, split == T)
accident_test <- subset(accident, split == F)
nrow(accident_test)
nrow(accident_train)
x <- accident_test
x$Aircraft.Damage <- NULL


library(e1071)
classifierr <- svm(formula = Aircraft.Damage ~ Injury.Severity, data = accident_train, 
                   type = "C-classification", kernel = "radial")
pred_accident <- predict(classifierr, newdata = x)

cm <- table(accident_test$Aircraft.Damage,pred_accident)



accident$Report.Status


accident$Airport.Code <- as.character(accident$Airport.Code)


accident$Injury.Severity
