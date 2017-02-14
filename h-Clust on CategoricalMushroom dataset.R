mushroomRaw <- read.csv("mushrooms.csv")
head(mushroomRaw)
summary(mushroomRaw)
str(mushroomRaw)
library(ade4)
binData <- acm.disjonctif(mushroomRaw)

binMushRoom <- dist(binData, method = "binary")
eucidMusroom <- dist(binData, method = "euclidean")

hclusMushroom <- hclust(binMushRoom, method = "ward.D2")
plot(hclusMushroom)

hclusMushRoomEucid <- hclust(eucidMusroom, method = "ward.D2")
plot(hclusMushRoomEucid)
#View from Dendogram looks like 3 clusters

xmmBi <- cutree(hclusMushroom,3)
y <- mushroomRaw[xmmBi == 1,]
summary(y)

y <- mushroomRaw[xmmBi == 2,]
summary(y)

y <- mushroomRaw[xmmBi == 3,]
summary(y)

library(klaR)
km = kmodes(mushroomRaw, 3)
table(xmmBi,km$cluster)

