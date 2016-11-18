#Linear Regression on BikeShare

bike_df <- read.csv("bikeshare.csv")
head(bike_df)
summary(bike_df)
str(bike_df)
bike_df$season <- factor(bike_df$season)

library(ggplot2)
g <- ggplot(bike_df)
#g+geom_point(aes(y=bike_df$count, x= bike_df$temp,color = bike_df$temp), alpha = 0.6)

ggplot(bike_df,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

library(corrplot)
cor(bike_df$temp, bike_df$count)
 cor_data<- cor(bike_df[,c('temp','count')])
corrplot(cor_data, method = "color")


ggplot(bike_df) + geom_boxplot(aes(x=bike_df$season, y = bike_df$count, color = bike_df$season)) + theme_bw()


temp.model <- lm(count~temp,bike_df)
temp <- lm(count~., bike_df)

summary(temp.model)

