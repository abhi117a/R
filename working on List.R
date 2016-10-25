 util<- read.csv("Machine-Utilization.csv")

 
 
 summary(util)
 str(util)
 util$Utilization <- (1-util$Percent.Idle)
 
 util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")
 
 #Rearrange the Columns
 
 util$Timestamp <- NULL
 util <- util[,c(4,1,2,3)]
 head(util, 12)
 util
 ####################
 summary(util)
 RL1 <- util[util$Machine == "RL1",]
 summary(RL1)
 RL1$Machine <- factor(RL1$Machine)
 
 RL1_Stats <- c(min(RL1$Utilization, na.rm = T), mean(RL1$Utilization, na.rm = T),max(RL1$Utilization, na.rm = T))
 
 
 #### Check if utilization ever reduced less than 90% for Utilization
 
 which(RL1$Utilization < 0.90)
 
 util_under_90_flag <- length(which(RL1$Utilization < 0.90)) >0
 util_under_90_flag
 list_RL1 <- list("RL1", RL1_Stats,util_under_90_flag)
 list_RL1
 
 #NAming the List
 names(list_RL1) <- c("Machine","Stats","LowThreshold")
 
 #Accessing the List
 
 list_RL1
 
 list_RL1[1]
 
 list_RL1[[1]]
 
 list_RL1$Machine
 
 list_RL1[2]
 
 list_RL1[[2]][3]
 
 list_RL1$Stats[3]
 
 #### Adding Values to the List
 
 list_RL1[4] <- "NewInformation"
 list_RL1[[4]][1] <- "TestVal1"
 list_RL1$UnknowHours <- RL1[is.na(RL1$Utilization),"PosixTime"]
 
 #Remove a componenet
 list_RL1[[4]] <- NULL
 
 #Add a component to List again
 
 list_RL1$Data <- RL1
 summary(list_RL1)
 str(list_RL1)
 
 #Subsetting a List
 
 list_RL1[1:3]
 subset_List <- list_RL1[c("Machine","Stats")]
 
 list_RL1[[2]][1:2]
 
 #ggplot
 #for facet Grid always use direct Coulmn name without any quotes or using $ sign
 
 library(ggplot2)
g <- ggplot(data=util)
  myplot <- g + geom_line(aes(x=util$PosixTime,y=util$Utilization,color = util$Machine)) +
  facet_grid(Machine~.) + geom_hline(yintercept = 0.9, size = 1.2,color = "Gray",linetype=3)
 
 list_RL1$Plot <-  myplot
 
 list_RL1
 
 
 
 
 