#Visualization
#install.packages("ggplot2movies")
library(ggplot2movies)
library(ggplot2)

df <- movies
head(df)


g <- ggplot(df)

g + geom_histogram(aes(x=df$CritiqueRating, fill = df$Genere),linetype = "dotted", color = "black") + xlab("Rating")+
  ylab("Count")+ggtitle("Ratings based on Genere") + theme_bw()



g + geom_histogram(aes(x=df$CritiqueRating,fill = ..count..), color = "black") + xlab("Rating")+
  ylab("Count")+ggtitle("Ratings based on Genere") + theme_bw()



g + geom_histogram(aes(x=df$CritiqueRating,fill = ..count..), color = "black") + xlab("Rating")+
  ylab("Count")+ggtitle("Ratings based on Genere") + theme_bw() + scale_fill_gradient(low = "white", high = "orange")+
  geom_density(color = "green")
