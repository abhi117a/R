vote <- read.csv("CongressialVoting.csv")
head(vote)
voteClean <- vote[,-1]
str(voteClean)
library(plyr)


voteClean$handicapped.infants <-revalue(voteClean$handicapped.infants, c("?" = "n"))
voteClean$water.project.cost.sharing <- revalue(voteClean$water.project.cost.sharing,c("?" = "n"))

voteClean$adoption.of.the.budget.resolution <- revalue(voteClean$adoption.of.the.budget.resolution, c("?" = "n"))
voteClean$physician.fee.freeze <- revalue(voteClean$physician.fee.freeze,c("?" = "n"))
voteClean$el.salvador.aid <- revalue(voteClean$el.salvador.aid,c("?" = "n"))
voteClean$religious.groups.in.schools <- revalue(voteClean$religious.groups.in.schools,c("?" = "n"))
voteClean$anti.satellite.test.ban <- revalue(voteClean$anti.satellite.test.ban,c("?" = "n"))
voteClean$aid.to.nicaraguan.contras <- revalue(voteClean$aid.to.nicaraguan.contras,c("?" = "n"))
voteClean$mx.missile <- revalue(voteClean$mx.missile,c("?" = "n"))
voteClean$immigration <- revalue(voteClean$immigration,c("?" = "n"))
voteClean$synfuels.corporation.cutback <- revalue(voteClean$synfuels.corporation.cutback,c("?" = "n"))
voteClean$education.spending <- revalue(voteClean$education.spending,c("?" = "n"))
voteClean$superfund.right.to.sue <- revalue(voteClean$superfund.right.to.sue,c("?" = "n"))
voteClean$crime <- revalue(voteClean$crime,c("?" = "n"))
voteClean$duty.free.exports <- revalue(voteClean$duty.free.exports,c("?" = "n"))
voteClean$export.administration.act.south.africa <- revalue(voteClean$export.administration.act.south.africa,c("?" = "n"))

#voteClean <- as.data.frame(unclass(voteClean))
#Applying Distance Metric
voteClean <- as.data.frame(voteClean)
library(ade4)
voteBinData <- acm.disjonctif(voteClean)
head(voteBinData)

voteDistanceMetric <- dist(voteBinData,method = "binary")
fitVote <- hclust(d = voteDistanceMetric, method = "ward.D2")
plot(fitVote, main = "Vote Dataset")
fitVote$order
cm <- table(fitVote$labels,vote[,1])

#WholeSale
wholeSale <- read.csv("Wholesale customers data.csv")
head(wholeSale)
str(wholeSale)
wholeSale$Channel <- as.factor(wholeSale$Channel)
wholeSale$Region <- as.factor(wholeSale$Region)

library(cluster)
wholeSaleDaisy <- daisy(wholeSale)
fitWholeSale <- hclust(d = wholeSaleDaisy,method = "ward.D2")
plot(fitWholeSale, main = "WholeSale")
