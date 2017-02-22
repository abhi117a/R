setwd("E:/R")
kar <- read.csv("autos.csv") 
kar <- kar[,-1]
kar$nrOfPictures <- NULL
kar$lastSeen <- NULL
kar$dateCreated <- NULL
head(kar)
str(kar)
summary(kar)

#Updated
kar1 <- kar[,-1:-3]
str(kar1)
kar1$abtest <- NULL
kar1$vehicleType <- NULL
kar1$gearbox <- NULL
kar1$model <- NULL
kar1$monthOfRegistration <- NULL
kar1$fuelType <- NULL
kar1$brand <- NULL
kar1$notRepairedDamage <- NULL
#lmR <- lm(formula =  price~., data = kar)

library(h2o)
h2o.init(nthreads = -1)
localH2O <- h2o.init(ip = '127.0.0.1', port =54321)

#carsPath <- system.file("E","R","autos.csv", package = "h2o")
#kars.hex <- h2o.importFile(localH2O, path = "E:/R/autos.csv",destination_frame = "kars.hex", parse = FALSE)
#head(kars.hex)
#summary(kars.hex)

karsh2o <- as.h2o(kar)
summary(karsh2o)
str(karsh2o)
lmR <- lm(formula =  price~., data = karsh2o)
h2o.glm(y = "price", x = c("offerType","abtest","vehicleType","yearOfRegistration"
                           ,"gearbox","powerPS","model","kilometer","fuelType"
                           ,"brand","notRepairedDamage","postalCode"), training_frame = karsh2o, 
        family = "gaussian", nfolds = 10, alpha = 0.5)

#updated
karsh2o1 <- as.h2o(kar1)
summary(karsh2o1)
#str(karsh2o1)
#lmR1 <- lm(formula =  price~., data = karsh2o)
fit <- h2o.glm(y = "price", x = c("yearOfRegistration","powerPS","kilometer","postalCode"), 
        training_frame = karsh2o1, 
        family = "gaussian", nfolds = 10, alpha = 0.5)
test <- c("2016","96.05","14956","30549")
testh2o <- as.h2o(test)
pred <- h2o.predict(fit,testh2o)
