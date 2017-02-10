setwd("C:/Users/admin/Documents")
tsunami <- read.csv("tsunami.csv", na.strings = "")
head(tsunami)
tsunami[is.na(tsunami)] = 0
summary(tsunami)
str(tsunami)
ncol(tsunami)
#Cleaning the Data
tsunami$SOURCE_ID <- NULL
tsunami$MINUTE <- NULL
tsunami[["HOUR"]] <- ordered(cut(tsunami[["HOUR"]], c(-1,6,12,18,100)), labels = c("Night","Morning","Noon","Evening"))
tsunami$MONTH <- as.factor(tsunami$MONTH)
tsunami[["DAY"]] <- ordered(cut(tsunami[["DAY"]], c(-1,15,32)), labels = c("FortNight1","FortNight2"))
tsunami$CAUSE <- as.factor(tsunami$CAUSE)
tsunami$VALIDITY <- as.factor(tsunami$VALIDITY)
tsunami$FOCAL_DEPTH <- as.factor(tsunami$FOCAL_DEPTH)
tsunami[["PRIMARY_MAGNITUDE"]] <- ordered(cut(tsunami[["PRIMARY_MAGNITUDE"]], c(-1,2.5,5.5,11)), labels = c("Low","Med","High"))
tsunami$REGION_CODE <- NULL
#tsunami$COUNTRY <- as.factor(tsunami$COUNTRY)
tsunami$LATITUDE <- NULL
tsunami$LONGITUDE <- NULL
tsunami$MAXIMUM_HEIGHT <- as.factor(tsunami$MAXIMUM_HEIGHT)
tsunami$MAGNITUDE_ABE <- as.factor(tsunami$MAGNITUDE_ABE)
tsunami$MAGNITUDE_IIDA <- as.factor(tsunami$MAGNITUDE_IIDA)
tsunami$INTENSITY_SOLOVIEV <- as.factor(tsunami$INTENSITY_SOLOVIEV)
tsunami$MISSING <- NULL
tsunami$MISSING_ESTIMATE <- NULL
tsunami$INJURIES <- NULL
tsunami$INJURY_ESTIMATE <- NULL
tsunami$FATALITIES <- NULL
tsunami$FATALITY_ESTIMATE <- NULL
tsunami$DAMAGE_MILLIONS_DOLLARS <- NULL
tsunami$DAMAGE_ESTIMATE <- NULL
tsunami$HOUSES_DAMAGED <- NULL
tsunami$HOUSE_DAMAGE_ESTIMATE <- NULL
tsunami$HOUSES_DESTROYED <- NULL
tsunami$HOUSE_DESTRUCTION_ESTIMATE <- NULL
tsunami$ALL_MISSING <- NULL
tsunami$MISSING_TOTAL <- NULL
tsunami$ALL_INJURIES <- NULL
tsunami$INJURY_TOTAL <- NULL
tsunami$ALL_FATALITIES <- NULL
tsunami$FATALITY_TOTAL <- NULL
tsunami$ALL_DAMAGE_MILLIONS <- NULL
tsunami$DAMAGE_TOTAL <- NULL
tsunami$ALL_HOUSES_DAMAGED <- NULL
tsunami$HOUSE_DAMAGE_TOTAL<- NULL
tsunami$ALL_HOUSES_DESTROYED <- NULL
tsunami$HOUSE_DESTRUCTION_TOTAL <- NULL
tsunami$WARNING_STATUS <- NULL
tsunami$YEAR <- ordered(as.factor(tsunami$YEAR))
summary(tsunami)
str(tsunami)


library(arules)
rules <- apriori(tsunami)
#rule to find out during which time maximum tsunamis occured
rules <- apriori(tsunami, parameter = list(minlen=1, supp=0.12, conf=0.8), appearance = list(rhs=c("HOUR=Night", "HOUR=Morning","HOUR=Noon", "HOUR=Evening"), default = "lhs"))

#rules to find out which was the biggest cause
rules <- apriori(tsunami, parameter = list(minlen=1, supp=0.12, conf=0.8), appearance = list(rhs=c("CAUSE=1", "CAUSE=2","CAUSE=3", "CAUSE=4", "CAUSE=5", "CAUSE=6", 
                                                                                                    "CAUSE=7", "CAUSE=8", "CAUSE=9", "CAUSE=10", "CAUSE=11", "CAUSE=0"),
                                                                                              default = "lhs"))


rules.sorted <- sort(rules,by="lift")


head(inspect(rules))
library(arulesViz)
plot(rules)

