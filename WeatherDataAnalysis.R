getwd()
setwd(".//Weather Data")
Chicago <- read.csv("Chicago-F.csv",row.names = 1)
NewYork <- read.csv("NewYork-F.csv",row.names = 1)
Houston <- read.csv("Houston-F.csv",row.names = 1)
SanFrancisco <- read.csv("SanFrancisco-F.csv",row.names = 1)

Chicago <- as.matrix(Chicago)
NewYork <- as.matrix(NewYork)
Houston <- as.matrix(Houston)
SanFrancisco <- as.matrix(SanFrancisco)

Weather <- list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)
Weather

cbind( apply(Chicago, 1, mean),
apply(NewYork, 1, mean),
apply(Houston, 1, mean),
apply(SanFrancisco, 1, mean))


rbind( apply(Chicago, 1, mean),
       apply(NewYork, 1, mean),
       apply(Houston, 1, mean),
       apply(SanFrancisco, 1, mean))

lapply(Weather, t)

lapply(Weather, rbind, NewRow = 1:12)

lapply(Weather, rowMeans)

#######

lapply(Weather, "[",1,1)

lapply(Weather, "[",1,)

lapply(Weather, "[",,3)

##Apply ownFunction

lapply(Weather, function(z) z[1,]-z[2,])
lapply(Weather, function(z) round((z[1,]-z[2,])/z[2,],2))
Weather
lapply(Weather, function(x) round(x[4,]/x[3,],2))

###################

sapply(Weather, rowMeans)
sapply(Weather, function(x) x[1,]-x[2,]/x[2,])
sapply(Weather, function(z) round(z[4,]/z[3,],2))
