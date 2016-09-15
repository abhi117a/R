#Creating Random Variables of Exponential distribution.
draws<-replicate(10000,(max(rexp(3,1/5))))

#Ploting Histogram
hist(draws)

#Finding Expected Value
mean(draws)

#PDF function
func <- function(x){
  0.6 * (((1-exp(-0.2*x))^2)*exp(-0.2*x))
}

#Plotting Curve of PDF function
curve(func(x),8,60,add = FALSE,col = 'blue')

#Finding the Probability of program to complete more than 20 minutes
count <- 0
for (v in draws)
{
  if (v > 20.00){
    count <- count +1
  }
}

#Generating Expected Value
probablity <- count/10000 
print (probablity)

