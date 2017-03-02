library(rsparkling)
options(rsparkling.sparklingwater.version = "2.0.3")

library(h2o)
library(dplyr)
library(sparklyr)


sc <- spark_connect(master = "local",version = "2.0.0")
arrythimia <- read.table("arrhythmia.txt",sep=",")
#head(arrythimia[,280])

# Col 280 is the Response Value which we will try to predict. We will convert it into factors 
#as such Each Number denotes the class.

arrythimia[,280] = as.factor(arrythimia[,280])
#Rest Predictor Columns convert it into Numerical values

arrythimia[,-280] = lapply(arrythimia[,-280], as.numeric)
#str(arrythimia)
#str(arrythimia[,280])

colnames(arrythimia)[280] <- "class"

library(ggplot2)
g <- ggplot(arrythimia)
g+ geom_histogram(aes(as.numeric(arrythimia$class)))

g + geom_bar(aes(x=arrythimia$class),
             fill = "red",color = "black", alpha = 0.8)+theme_dark()

#library(corrgram)
#corrgram(arrythimia)

#Lets Make 2 factors i.e. 1 = Healthy 2 = Not Healthy

arrythimia$diagnosis <- ifelse(arrythimia$class == 1 , "healthy", "arrythmia")
head(arrythimia$diagnosis)

g + geom_bar(aes(x=arrythimia$diagnosis), 
             fill = "red",color = "black", alpha = 0.8)+ theme_dark()

##Variance

library(matrixStats)

colvars <- data.frame(feature = colnames(arrythimia[-c(280, 281)]),
                      variance = colVars(as.matrix(arrythimia[-c(280, 281)])))

subset(colvars, variance > 50) %>%
  mutate(feature = factor(feature, levels = colnames(arrythimia[-c(280, 281)]))) %>%
  ggplot(aes(x = feature, y = variance)) +
  geom_bar(stat = "identity", fill = "navy", alpha = 0.7)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

