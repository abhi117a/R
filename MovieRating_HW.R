#MOvieRating HW

Movies <- read.csv("Section6-Homework-Data.csv")

Movies$Day.of.Week <- NULL
Movies$Release.Date <- NULL
Movies$Director <- NULL
Movies$Runtime..min. <- NULL

head(Movies)

str(Movies)
Movies$Genre <- factor(Movies$Genre)
Movies$Studio <- factor(Movies$Studio)

head(Movies)


myDF <- data.frame(Movies$Genre,Movies$Movie.Title,Movies$Studio,Movies$Budget...mill.,Movies$Gross...US)
colnames(myDF) <- c("Genere","Title","Studio","Budget","GrossUSA")

myDF$Genere <- factor(myDF$Genere)
myDF1 <- myDF [myDF$Genere != "horror" & myDF$Genere != "fantasy" & myDF$Genere != "biography" & myDF$Genere != "thriller"& myDF$Genere != "sci-fi" & myDF$Genere != "musical"& myDF$Genere != "crime"& myDF$Genere != "romance"& myDF$Genere != "mystery" & myDF$Genere != "documentary",]


#filt <- (myDF1$Studio = "Buena Vista Studios") | (myDF1$Studio = "Fox") | (myDF1$Studio = "Paramount Pictures") | (myDF1$Studio = "WB") | (myDF1$Studio = "Sony") | (myDF1$Studio = "Universal")
#filt1 <- (myDF1$Studio != "Colombia Pictures") & (myDF1$Studio != "TriStar") 
#myDF1 <- myDF1[myDF1$Studio = "Buena Vista Studios" | myDF1$Studio = "Fox" | myDF1$Studio = "Paramount Pictures" | myDF1$Studio = "WB" | myDF1$Studio = "Sony" | myDF1$Studio = "Universal",]


myDF2 <- myDF1[myDF1$Studio != "Colombia Pictures" & myDF1$Studio != "TriStar" & myDF1$Studio != "Lionsgate"& myDF1$Studio != "Disney"& myDF1$Studio != "DreamWorks" & myDF1$Studio != "Fox Searchlight Pictures" & myDF1$Studio != "Gramercy Pictures" & myDF1$Studio != "Orion" & myDF1$Studio != "Weinstein Company" & myDF1$Studio != "WB/New Line" & myDF1$Studio != "Vestron Pictures" & myDF1$Studio != "UA Entertainment" & myDF1$Studio != "Summit Entertainment" & myDF1$Studio != "StudioCanal" & myDF1$Studio != "Sony Pictures Classics" & myDF1$Studio != "Screen Gems" & myDF1$Studio != "Revolution Studios" & myDF1$Studio != "Path_Distribution" & myDF1$Studio != "Pacific Data/DreamWorks" & myDF1$Studio != "New Market Films" & myDF1$Studio != "New Line Cinema" & myDF1$Studio != "MiraMax" & myDF1$Studio != "MGM" & myDF1$Studio != "Lionsgate/Summit" & myDF1$Studio != "Lionsgate Films" & myDF1$Studio != "IFC" & myDF1$Studio != "Dimension Films" & myDF1$Studio != "Path_ Distribution" & myDF1$Studio != "Sony Picture Classics",]

#unique(myDF2[myDF2$Studio,])
#is.data.frame(myDF2) 

head(myDF2)

library(ggplot2)
q <- ggplot(data = myDF2,aes(x=Genere,y=GrossUSA)) + geom_jitter(aes(color = Studio,size=Budget))+ geom_boxplot(aes(alpha=0.5),outlier.colour = NA)

q+ xlab("Genre") + ylab("Gross USA") + ggtitle("Domestic Gross % VS Genre")
