gammaSample <- replicate(10000, (max(rgamma(3, 1, 5)))) #Creating Random Variables of Gamma distribution.
draws<- 1/gammaSample #Converting Lambda into Time.
curve(dgamma(x,1,5),add = FALSE,col = 'blue') #Plot Curve 
hist(draws) #Plot the histogram
expectedValue <- mean(draws) #Getting Expected Value
print (expectedValue) #Printing the expected Value(Mean)
#Logic for counting the mean for time > 20mins
count <- 0
for (v in draws)
{
   if (v > 20.00){
     count <- count +1
     }
}
probablity <- count/10000 #Generating Expected Value
print (probablity) #Printing the Expected Value


