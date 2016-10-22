z <- factor(c(113,112,885,9,45))
typeof(z)
z

z <- as.numeric(as.character(z))
z
typeof(z)
###
# USING gsub() to replace the patterns in entire coulumns. we can use sub() to replace for one instance as well
# used na,strings() to replace empty data space with NA
data <- read.csv("Future-500.csv",na.strings = c(""))
head(data)
str(data)
summary(data)


data$Expenses <- gsub(" Dollars", "",data$Expenses)
data$Expenses <- gsub(",","",data$Expenses)
#Converting into numeric from character for Mathematical manuplation
data$Expenses <- as.numeric(data$Expenses) 

data$Revenue <- gsub("\\$","",data$Revenue)
data$Revenue <- gsub(",","",data$Revenue)
data$Revenue <- as.numeric(data$Revenue)


data$Growth <- gsub("%","",data$Growth)
data$Growth <- as.numeric(data$Growth)


#Missing Data
head(data,24)
#complete.cases() provide true false vector for every row which has data completely filled and having NA respectively
complete.cases(data)
#negation of complete.cases() provides data rows which has NA in it
data[!complete.cases(data),]

#Which() function helps to remove data with NA values while filtering the data

data[data$Employees==45,]
data[which(data$Employees==45),]

#is.na() function helps to remove NA values
data[!complete.cases(data),]
data <- data[!is.na(data$Industry),] #check the colums with NA values removes them while using ! and updates the DataFrame

rownames(data) <- NULL # Resetting the row indexes


data[!complete.cases(data),]

data[is.na(data$State),]
data[is.na(data$State) & (data$City == "New York"),]
data[is.na(data$State) & (data$City == "New York"),"State"] <- "NY"

data[is.na(data$State),]
data[is.na(data$State) & (data$City) == "San Francisco", "State"] <- "CA"

#Median Imputation Method

data[!complete.cases(data),]

median(data[,"Employees"],na.rm = T)
mean(data[,"Employees"], na.rm = T)
median_employee_retail <- median(data[data$Industry == "Retail", "Employees"], na.rm = T)
median_employee_retail

data[is.na(data$Employees) & (data$Industry=="Retail"),"Employees"] <- median_employee_retail

data[3,]

median_Emp_FinService <- median(data[data$Industry == "Financial Services","Employees"],na.rm = T)
data[is.na(data$Employees)&(data$Industry == "Financial Services"),"Employees"] <- median_Emp_FinService

