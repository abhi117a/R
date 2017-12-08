library(tidyverse)
library(OneR)
library(magrittr)
library(sos)
library(caret)
library(doParallel)
library(rpart)
library(rpart.plot)
library(e1071)
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


#Now I am running the OneR models. During model building, the chosen attribute/feature with highest accuracy along with the top 7 
#features decision rules and accuracies are printed. Unfortunately, this information is not saved in the model object; 
#this would have been nice in order to compare the importance of features across models later on.
#Here, all five models achieved highest prediction accuracy with the feature Economy GDP per capita.



for (i in 1:5) {
  data <- get(paste0("data_", i))
  print(model <- OneR(formula = Happiness.Score.l ~., data = data, verbose = TRUE))
  assign(paste0("model_", i), model)
}


#The function eval_model() prints confusion matrices for absolute and relative predictions, as well as accuracy, error and error rate reduction. 
#For comparison with other models, it would have been convenient to be able to extract these performance metrics directly from the eval_model object,
#instead of only the confusion matrix and values of correct/all instances and having to re-calculate performance metrics again manually.
  
for (i in 1:5) {
  model <- get(paste0("model_", i))
  eval_model(predict(model, test_data), test_data$Happiness.Score.l)
}


#Because I want to calculate performance measures for the different classes separately and like to have a more detailed look at the prediction probabilities I
#get from the models, I prefer to obtain predictions with type = "prob. While I am not looking at it here, this would also allow me to test different prediction 
#thresholds.


for (i in 1:5) {
  model <- get(paste0("model_", i))
  pred <- data.frame(model = paste0("model_", i),
                     sample_id = 1:nrow(test_data),
                     predict(model, test_data, type = "prob"),
                     actual = test_data$Happiness.Score.l)
  pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
  pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
  pred$pred_prob <- NA
  
  for (j in 1:nrow(pred)) {
    pred[j, "pred_prob"] <- max(pred[j, 3:5])
  }
  
  if (i == 1) {
    pred_df <- pred
  } else {
    pred_df <- rbind(pred_df, pred)
  }
}


#First, I am building a decision tree with the rpart package and rpart() function. This, we can plot with rpart.plot().
#Economy GDP per capita is the second highest node here, the best predictor here would be health and life expectancy.


set.seed(42)
fit <- rpart(Happiness.Score.l ~ .,
             data = train_data,
             method = "class",
             control = rpart.control(xval = 10), 
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)

#In order to compare the models, I am producing the same output table for predictions from this model and combine it with the table from the OneR models.

pred <- data.frame(model = "rpart",
                   sample_id = 1:nrow(test_data),
                   predict(fit, test_data, type = "prob"),
                   actual = test_data$Happiness.Score.l)
pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
pred$pred_prob <- NA

for (j in 1:nrow(pred)) {
  pred[j, "pred_prob"] <- max(pred[j, 3:5])
}


pred_df_final <- rbind(pred_df,
                       pred)
set.seed(42)
model_rf <- train(Happiness.Score.l ~ .,
                         data = train_data,
                         method = "rf",
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 10, 
                                                  repeats = 5, 
                                                  verboseIter = FALSE))


#The varImp() function from caret shows us which feature was of highest importance for the model and its predictions.
#Here, we again find Economy GDP per captia on top.


varImp(model_rf)

pred <- data.frame(model = "rf",
                   sample_id = 1:nrow(test_data),
                   predict(model_rf, test_data, type = "prob"),
                   actual = test_data$Happiness.Score.l)
pred$prediction <- colnames(pred)[3:5][apply(pred[, 3:5], 1, which.max)]
pred$correct <- ifelse(pred$actual == pred$prediction, "correct", "wrong")
pred$pred_prob <- NA

for (j in 1:nrow(pred)) {
  pred[j, "pred_prob"] <- max(pred[j, 3:5])
}


pred_df_final <- rbind(pred_df_final,
                       pred)
