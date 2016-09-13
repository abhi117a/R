gammaSample <- replicate(10000, (max(rgamma(3, 1, 5)))) #Creating Random Variables of Gamma distribution.
draws<- 1/gammaSample #Converting Lambda into Time.
expectedValue <- mean(draws) #Getting Expected Value
count <- 0
for (v in draws)
{
   if (v > 20.00){
     count <- count +1
     }
}
probablity <- count/10000
print (probablity)

#hist(draws)
#curve(dgamma(x,1,5),add = TRUE,col = 'blue')

