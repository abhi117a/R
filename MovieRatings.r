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


#------Starting Layer Tips

t <- ggplot(data=movies,aes(x=AudienceRating))
t+geom_histogram(binwidth = 10,fill = "Black",color ="REd")

#different way

t <- ggplot(data=movies)
t+geom_histogram(binwidth = 5,aes(x=AudienceRating),fill="Black",color ="Green")

#hist for critiq rating

t <- ggplot(data=movies)
t+geom_histogram(binwidth = 5,aes(x=CritiqueRating),fill="Pink",color ="Black")


#Geom_Smooth

t <- ggplot(data=movies,aes(x=CritiqueRating,y=AudienceRating,color = Genere)) 

t + geom_point() + geom_smooth(fill=NA)


#boxPlots

u <- ggplot(data=movies,aes(x=Genere, y=AudienceRating,color= Genere))

u+geom_boxplot(size =1.2)

#Jitter

u+geom_boxplot(size =1.2) + geom_jitter()

#jitter way 2

u+geom_jitter()+geom_boxplot(size =1.2, alpha=0.5)

#CritiqueRating BoxPlot

g <- ggplot(data = movies,aes(x=Genere,y=CritiqueRating, color = Genere))

g+geom_boxplot() + geom_jitter()

g+geom_jitter()+geom_boxplot(alpha=0.5)


#using facet

f <- ggplot(data= movies, aes(x=BudgetMillions))
f+geom_histogram(binwidth = 10,aes(fill = Genere),color = "Black") +
  facet_grid(Genere~., scales="free")

#ScatterPlot

s <- ggplot(data = movies,aes(x=CritiqueRating,y=AudienceRating,color=Genere))
s+geom_point(size=3)

#facet

s+geom_point(size=3) + facet_grid(Genere~.)

s+geom_point(size=3) + facet_grid(.~Year)

s+geom_point(size=3) + facet_grid(Genere~Year)

s+geom_point(size=3) + facet_grid(Genere~Year) + geom_smooth()

s+geom_point(aes(size=BudgetMillions)) + facet_grid(Genere~Year) + geom_smooth()

#------Coordinates

m <- ggplot(data=movies,aes(x=CritiqueRating,y=AudienceRating,color=Genere,size=BudgetMillions))

m+geom_point()+xlim(50,100)+ylim(50,100)

n <- ggplot(data = movies,aes(x=BudgetMillions))
n + geom_histogram(binwidth = 10,aes(fill=Genere),color = "Black") + coord_cartesian(ylim = c(0,50))

s <- ggplot(data = movies,aes(x=CritiqueRating,y=AudienceRating,color=Genere))

s+geom_point(aes(size=BudgetMillions)) + facet_grid(Genere~Year) + geom_smooth() + coord_cartesian(ylim = c(0,100))
