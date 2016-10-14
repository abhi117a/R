movies <- read.csv("Movie-Ratings.csv")
head(movies)
#changing columnames as the file has longer names
colnames(movies) <- c("Film","Genere","CritiqueRating","AudienceRating","BudgetMillions","Year")
head(movies)

movies$Year <- factor(movies$Year) #provides the factors i.e. converts data into levels of data
str(movies) #Shows Structure of the data.
summary(movies) #gives summary

#Building Plot using ggplot2 library

library(ggplot2)
p <- ggplot(movies, aes(x=CritiqueRating,y=AudienceRating,colour=Genere, size = BudgetMillions))

ggplot(movies, aes(x=CritiqueRating,y=AudienceRating,colour=Genere, size = BudgetMillions)) +
  geom_count()

p + geom_point()

#overRiding AES

q <- ggplot(movies, aes(x=CritiqueRating,y=AudienceRating,colour=Genere, size = BudgetMillions))
q + geom_point(aes(size=CritiqueRating))

q + geom_point(aes(color=BudgetMillions))

q + geom_point(aes(x=BudgetMillions)) + xlab("BudgetMillions")

#HistoGram

s <- ggplot(data=movies,aes(x=BudgetMillions))

s+geom_histogram(binwidth = 10,aes(fill=Genere),color="Black")


#Density

s+geom_density(aes(fill=Genere))

s+geom_density(aes(fill=Genere),position = "stack")
