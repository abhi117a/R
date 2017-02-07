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
