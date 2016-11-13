#HC clustering
hc <- read.csv("Mall_Customers.csv")
head(hc)
library(dplyr)
x <- select(hc,4:5)
#creating Dendrogram
dendogram <- hclust(dist(x,method = "euclidean"), method = 'ward.D')
plot(dendogram, xlab = "Customer", ylab = "Euclidean distance")

#Fitting the HC clustering to mall datatset

y_hc <- cutree(dendogram,5)
