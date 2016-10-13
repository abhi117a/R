#Read csv file
trees = read.csv("trees.csv")

#Calculate Natural Logs for later usage
logGirth = log(trees$Girth)
logVolume = log(trees$Volume)
logHeight = log(trees$Height)

#Histograms
par(mfrow=c(1,3), oma=c(0,0,2,0))
hist(trees$Girth, main="Girth", xlab="Girth", col="black",border = "white")
hist(trees$Height, main="Height", xlab="Height",col="black",border = "white")
hist(trees$Volume, main="Volume", xlab="Volume",col="black",border = "white")
mtext("Histogram", outer=TRUE, cex=1.5)

#5 point summary
par(mfrow=c(1,1),oma = c(0,0,0,0))
boxplot(trees,main="5 point summary", range=0)
boxplot(trees, main = "5 point summary for IQR",range=1.5)

#Q-Q plots
par(mfrow = c(1,3),oma=c(0,0,2,0))
qqnorm(trees$Girth,main="Girth")
qqline(trees$Girth)
qqnorm(trees$Volume,main="Volume")
qqline(trees$Volume)
qqnorm(trees$Height,main="Height")
qqline(trees$Height)
mtext("Normal Q-Q plot", outer=TRUE, cex=1.5)

#Scatter plots
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(trees$Girth, trees$Volume, main = "Scatterplot Girth Vs Volume", xlab = "Girth", ylab = "Volume")
lines(lowess(trees$Girth,trees$Volume), col="blue")
abline(lm(trees$Volume~trees$Girth), col="red")

plot(trees$Volume, trees$Height, main = "Scatterplot Volume Vs Height", xlab = "Volume", ylab = "Height")
lines(lowess(trees$Volume,trees$Height), col="blue")
abline(lm(trees$Height~trees$Volume), col="red")

plot(trees$Height, trees$Girth, main = "Scatterplot Height Vs Girth", xlab = "Height", ylab = "Girth")
lines(lowess(trees$Height,trees$Girth), col="blue")
abline(lm(trees$Girth~trees$Height), col="red")



#Histograms with Log
par(mfrow=c(1,3),oma=c(0,0,2,0))
hist(logGirth, main="LogGirth", xlab="Girth", col="black",border = "white")
hist(logHeight, main="LogHeight", xlab="Height", col="black",border = "white")
hist(logVolume, main="LogVolume", xlab="Volume", col="black",border = "white")
mtext("Histogram with Log", outer=TRUE, cex=1.5)

#5 point summary with Log
par(mfrow=c(1,3), oma=c(0,0,2,0))
boxplot(logGirth, range=0, main="LogGirth")
boxplot(logVolume, range = 0, main="LogVolume")
boxplot(logHeight,range = 0, main="LogHeight")
mtext("5 point summary", outer = TRUE, cex=1.5)

boxplot(logGirth, range=1.5, main="LogGirth")
boxplot(logVolume, range = 1.5, main="LogVolume")
boxplot(logHeight,range = 1.5, main="LogHeight")
mtext("5 point summary for IQR", outer = TRUE, cex=1.5)

#Q-Q plot with Log
par(mfrow = c(1,3), oma=c(0,0,2,0))
qqnorm(logGirth,main="LogGirth")
qqline(logGirth)
qqnorm(logVolume,main="LogVolume")
qqline(logVolume)
qqnorm(logHeight,main="LogHeight")
qqline(logHeight)
mtext("Normal Q-Q plot", outer = TRUE, cex=1.5)

#Scatter plots with Log
par(mfrow=c(1,1),oma=c(0,0,0,0))
plot(logGirth, logVolume, main = "Scatterplot LogGirth Vs LogVolume", xlab = "LogGirth", ylab = "LogVolume")
lines(lowess(logGirth,logVolume), col="blue")
abline(lm(logVolume~logGirth), col="red")


plot(logVolume, logHeight, main = "Scatterplot LogVolume Vs LogHeight", xlab = "LogVolume", ylab = "LogHeight")
lines(lowess(logVolume,logHeight), col="blue")
abline(lm(logHeight~logVolume), col="red")


plot(logHeight, logGirth, main = "Scatterplot LogHeight Vs LogGirth", xlab = "LogHeight", ylab = "LogGirth")
lines(lowess(logHeight,logGirth), col="blue")
abline(lm(logGirth~logHeight), col="red")