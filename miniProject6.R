#project6

df <- read.csv("prostate_cancer.csv")
head(df)
summary(df)
str(df)

library(ggplot2)
g <- ggplot(df)

#Scatter Plot PSA vs Subject
g+ geom_point(aes(x = df$psa, y = df$subject), size = 1.5, color = "RED")+ 
  xlab("PSA")+ylab("Subject")+ ggtitle("Scatter Plot PSA vs Subject") +theme_dark()

#Scatter Plot PSA vs Cancer Volume
g + geom_point(aes(x = df$psa, y = df$cancervol), size = 2, color = "RED")+ 
  xlab("PSA")+ylab("Cancer Volume")+ ggtitle("Scatter Plot PSA vs Cancer Volume") +theme_dark()

#Scatter Plot PSA vs Weight
g + geom_point(aes(x = df$psa, y = df$weight), size = 2, color = "RED", alpha = 0.6)+ 
  xlab("PSA")+ylab("Weight")+ ggtitle("Scatter Plot PSA vs Weight") +theme_dark()

#Scatter Plot PSA vs Age
g + geom_point(aes(x = df$psa, y = df$age), size = 2, color = "RED")+ 
  xlab("PSA")+ylab("Age")+ ggtitle("Scatter Plot PSA vs Age") +theme_dark()

#Scatter Plot PSA vs Benign prostatic hyperplasia
g + geom_point(aes(x = df$psa, y = df$benpros), size = 1.5, color = "Blue")+ 
  xlab("PSA")+ylab("Benign prostatic hyperplasia")+ 
  ggtitle("Scatter Plot PSA vs Benign prostatic hyperplasia") +theme_dark()

#Scatter Plot PSA vs Seminal vesicle invasion
g + geom_point(aes(x = df$psa, y = df$vesinv), size = 2, color = "RED")+ 
  xlab("PSA")+ylab("Seminal vesicle invasion ")+ 
  ggtitle("Scatter Plot PSA vs Seminal vesicle invasion ") +theme_dark()

#Scatter Plot PSA vs Capsular penetration
g + geom_point(aes(x = df$psa, y = df$capspen), size = 2, color = "RED")+ 
  xlab("PSA")+ylab("Capsular penetration")+ ggtitle("Scatter Plot PSA vs Capsular penetration") +theme_dark()

#Scatter Plot PSA vs Gleason score 
g + geom_point(aes(x = df$psa, y = df$gleason), size = 2, color = "RED")+ 
  xlab("PSA")+ylab("Gleason score ")+ ggtitle("Scatter Plot PSA vs Gleason score ") +theme_dark()

############################

library(corrgram)
library(corrplot)
cor_data <- cor(df)
corrgram(df)
corrplot(cor_data, method = "pie")


#splitting

confint()


