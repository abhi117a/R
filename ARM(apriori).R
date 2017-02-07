adultData <- read.csv("adultData.txt")
head(adultData)
str(adultData)
summary(adultData)
View(adultData)
adultData$fnlwgt <- NULL
adultData$education.num <- NULL
adultData[["age"]] <- ordered(cut(adultData[["age"]], c(15,25,45,65,100)), labels = c("Young","Middle", "Old", "Senior"))
adultData[["hours.per.week"]] <- ordered(cut(adultData[["hours.per.week"]], c(0,25,45,60,168)), labels =c("Part-time", "Full-time","Over-time","Veryhigh"))


adultData[[ "capital.gain"]] <- ordered(cut(adultData[[ "capital.gain"]], c(-Inf,0, median(adultData[["capital.gain"]][adultData[["capital.gain"]]>0]), Inf)), labels = c("None", "Low", "High"))
adultData[[ "capital.loss"]] <- ordered(cut(adultData[[ "capital.loss"]], c(-Inf,0, median(adultData[["capital.loss"]][adultData[["capital.loss"]]>0]), Inf)), labels = c("None", "Low", "High"))
summary(adultData)


adultData$workclass <- as.factor(adultData$workclass)
adultData$education <- as.factor(adultData$education)
adultData$marital.status <- as.factor(adultData$marital.status)
adultData$occupation <- as.factor(adultData$occupation)
adultData$relationship <- as.factor(adultData$relationship)
adultData$race <- as.factor(adultData$race)
adultData$sex <- as.factor(adultData$sex)
adultData$native.country <- as.factor(adultData$native.country)
adultData$income <- as.factor(adultData$income)


rules <- apriori(adultData, parameter = list(support = 0.5, conf = 0.85, target = "rules", minlen = 2))
rules.sorted = sort(rules,by="lift")
inspect(rules.sorted)
plot(rules)