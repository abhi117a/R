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
data[330,]

data[!complete.cases(data$Growth),]

median(data[,"Growth"],na.rm=T)
meadian_Construct_Growth <- median(data[data$Industry =="Construction","Growth"],na.rm=T)
data[is.na(data$Growth) & data$Industry == "Construction","Growth"] <- meadian_Construct_Growth
data[8,]

data[!complete.cases(data),]

median_Contruct_Rev <- median(data[data$Industry == "Construction","Revenue"], na.rm = T)
data[data$Industry == "Construction" & is.na(data$Revenue), "Revenue"] <- median_Contruct_Rev


median_Ind_Expenses <- median(data[data$Industry=="Construction","Expenses"], na.rm = T)
data[data$Industry== "Construction" & is.na(data$Expenses) & is.na(data$Profit),"Expenses"] <- median_Ind_Expenses


#Revenue-Expenses = Profit

data[is.na(data$Profit),"Profit"] <- data[is.na(data$Profit),"Revenue"] - data[is.na(data$Profit),"Expenses"] 
data[c(8,42),]

data[is.na(data$Expenses),"Expenses"] <- data[is.na(data$Expenses),"Revenue"] - data[is.na(data$Expenses),"Profit"]
data[15,]


library(ggplot2)
str(data)

p <- ggplot(data=data,aes(x = data$Revenue, y= data$Expenses, color= data$Industry)) 
p + geom_point(aes(size = data$Profit))

p + geom_smooth(fill=NA)


b <- ggplot(data = data, aes(x = data$Industry, y = data$Growth, color = data$Industry))
b+ geom_jitter(size = 1) +geom_boxplot(alpha = 0.5, size = 1, outlier.color = NA)




