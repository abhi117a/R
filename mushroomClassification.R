#MushRoom Classification
mushroom <- read.csv("cancerdata.csv")
head(mushroom,1)
str(mushroom)
mushroom[is.na(mushroom)]
lapply(mushroom, as.numeric)
mushroom <- mushroom <- as.numeric(unlist(mushroom))
head(mushroom)
library(corrplot)
cor_data <- cor(mushroom, use="pairwise.complete.obs")
corrplot(cor_data, method = "pie")

library(ggplot2)
g <- ggplot(mushroom)
g + geom_histogram(aes(x = mushroom$id))
hist(mushroom)
