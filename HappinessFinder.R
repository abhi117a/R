library(tidyverse)
library(OneR)
library(magrittr)
library(sos)
library(caret)
library(doParallel)
#library(binr)
data_16 <- read.table("2016.csv", sep = ",", header = TRUE)
data_15 <- read.table("2015.csv", sep = ",", header = TRUE)


common_feats <- colnames(data_16)[which(colnames(data_16) %in% colnames(data_15))]

# features and response variable for modeling
feats <- setdiff(common_feats, c("Country", "Happiness.Rank", "Happiness.Score"))
response <- "Happiness.Score"

# combine data from 2015 and 2016
data_15_16 <- rbind(select(data_15, one_of(c(feats, response))),
                    select(data_16, one_of(c(feats, response))))

#The response variable happiness score is on a numeric scale. 
#OneR could also perform regression but here, I want to compare classification tasks. 
#For classifying happiness, I create three bins for low, medium and high values of the happiness score. 
#In order to not having to deal with unbalanced data, I am using the bin() function from OneR with method = "content". 
#For plotting the cut-points, I am extracting the numbers from the default level names.

data_15_16$Happiness.Score.l <- bin(data_15_16$Happiness.Score, nbins = 3, method = "content")

intervals <- paste(levels(data_15_16$Happiness.Score.l), collapse = " ")
intervals <- gsub("\\(|]", "", intervals)
intervals <- gsub(",", " ", intervals)
intervals <- as.numeric(unique(strsplit(intervals, " ")[[1]]))

data_15_16 %>%
  ggplot() +
  geom_density(aes(x = Happiness.Score), color = "blue", fill = "blue", alpha = 0.4) +
  geom_vline(xintercept = intervals[2]) +
  geom_vline(xintercept = intervals[3])
#Now I am removing the original happiness score column from the data for modeling and rename the factor levels 
#of the response variable.

data_15_16 <- select(data_15_16, -Happiness.Score) %>%
  mutate(Happiness.Score.l = plyr::revalue(Happiness.Score.l, c("(2.83,4.79]" = "low", "(4.79,5.89]" = "medium", "(5.89,7.59]" = "high")))


#Because there are only 9 features in this small dataset, I want to explore them all individually before modeling. 
#First, I am plotting the only categorical variable: Region.
#This plots shows that there are a few regions with very strong biases in happiness: 
#People in Western Europe, Australia, New Zealand, North America, Latin American and the Caribbean tend to me in the high happiness group, 
#while people in sub-saharan Africa and Southern Asia tend to be the least happiest.



data_15_16 %>%
  ggplot(aes(x = Region, fill = Happiness.Score.l)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = unit(c(0, 0, 0, 1.5), "cm")) +
  scale_fill_brewer(palette = "Set1")

#The remaining quantitative variables show happiness biases to varying degrees: 
#e.g. low health and life expectancy is strongly biased towards low happiness, economic factors, 
#family and freedom show a bias in the same direction, albeit not as strong.


data_15_16 %>%
  gather(x, y, Economy..GDP.per.Capita.:Dystopia.Residual) %>%
  ggplot(aes(x = y, fill = Happiness.Score.l)) +
  geom_histogram(alpha = 0.7) +
  facet_wrap(~ x, scales = "free", ncol = 4) +
  scale_fill_brewer(palette = "Set1")

#While OneR could also handle categorical data, in this example, I only want to consider the quantitative features to show the differences between 
#OneR and other machine learning algorithms.


data_15_16 <- select(data_15_16, -Region)

#The algorithms I will compare to OneR will be run via the caret package.

# configure multicore

cl <- makeCluster(detectCores())
registerDoParallel(cl)

#I will also use caret’s createDataPartition() function to partition the data into training (70%) and test sets (30%).

set.seed(42)
index <- createDataPartition(data_15_16$Happiness.Score.l, p = 0.7, list = FALSE)
train_data <- data_15_16[index, ]
test_data  <- data_15_16[-index, ]

#OneR only accepts categorical features. Because we have numerical features, 
#we need to convert them to factors by splitting them into appropriate bins. 
#While the original OneR algorithm splits the values into ever smaller factors, this has been changed in 
#this R-implementation with the argument of preventing overfitting. We can either split the data 
#into pre-defined numbers of buckets (by length, content or cluster) or we can use the optbin() function to 
#obtain the optimal number of factors from pairwise logistic regression or information gain.

# default method length
data_1 <- bin(train_data, nbins = 5, method = "length")

# method content
data_2 <- bin(train_data, nbins = 5, method = "content")

# method cluster
data_3 <- bin(train_data, nbins = 3, method = "cluster")

# optimal bin number logistic regression
data_4 <- optbin(formula = Happiness.Score.l ~., data = train_data, method = "logreg")

# optimal bin number information gain
data_5 <- optbin(formula = Happiness.Score.l ~., data = train_data, method = "infogain")




  

