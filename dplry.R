#DPLYR
library(dplyr)
library(nycflights13)
head(flights)
summary(flights)
str(flights)
#filter and slice
head(filter(flights, month == 11, day == 12, carrier == "AA")) 
head(slice(flights, 1:10))
#arrange 
arrange(flights,desc(arr_time))
#select

head(select(flights,day,dep_time))
#distinct
distinct(select(flights,arr_time))
#mutate and transmute

mutate(flights,new_coll = arr_delay-dep_delay)
transmute(flights,new_coll = arr_delay-dep_delay)

#summarise
summarise(flights,sum(arr_time, na.rm = T))

#sample
sample_n(flights,10)
sample_frac(flights, 0.1)

#pipe

df <- mtcars

result <- df %>% filter(mpg>20) %>% sample_frac(0.2) %>% arrange(desc(mpg))
                                                            


