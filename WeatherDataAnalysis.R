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
