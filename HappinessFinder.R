library(tidyverse)

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
colnames(data_15_16)
head(data_15_16)
  

