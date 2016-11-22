df <- read.csv("bank_note_data.csv")
head(df)
str(df)

library(ggplot2)
g <- ggplot(df)
g+  geom_histogram(aes(x=df$Image.Var), color = "black")

g + geom_histogram(aes(df$Image.Skew), color = "black", fill = "red")

g + geom_histogram(aes(x = df$Class))

library(caTools)

split <- sample.split(df$Class, SplitRatio = 0.75)

train_bank <- subset(df, split == T)
test_bank <- subset(df, split == F)
nrow(test_bank)

library(neuralnet)

nn <- neuralnet(formula = Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train_bank, hidden = c(10), linear.output = F )

plot(nn)

predi <- compute(nn, test_bank[,-5])

head(predi$net.result)

predictions <- sapply(predi$net.result, round)

truVal <- test_bank$Class

tab <- as.data.frame(predictions,truVal)

head(tab)

ggplot(data = tab, aes(x=tab$predictions,y=truVal))
cm <-table(predictions,test_bank$Class)
