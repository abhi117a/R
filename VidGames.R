vidGame <- read.csv("vgsales.csv", na.strings = "")
vidGame <- utf8ToInt(vidGame)
head(vidGame)
str(vidGame)
levels(vidGame$Platform)

#Removing of Platforms other than PS4 XBOXone WiiU and PC
vidGame1 <- vidGame1[vidGame1$Platform!="WS",]
head(vidGame1)
str(vidGame1)

library(ggplot2)
g <- ggplot(vidGame1,aes(x = vidGame1$Platform, y = log(vidGame1$NA_Sales), color = vidGame1$Platform))
g +geom_jitter()+geom_boxplot(alpha = 0.6) 

g + geom_point(aes(x = log(vidGame1$NA_Sales), y = log(vidGame1$EU_Sales), color = vidGame1$Platform))

g <- ggplot(vidGame1,aes(x = vidGame1$Platform, y = log(vidGame1$EU_Sales), color = vidGame1$Platform))
g +geom_jitter()+geom_boxplot(alpha = 0.6) 


g <- ggplot(vidGame1)
g + geom_point(aes(x = vidGame1$NA_Sales*1000, y = vidGame1$Rank, color = vidGame1$Platform)) 

vidGame1$Rank

