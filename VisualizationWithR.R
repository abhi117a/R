library(ggplot2)
head(mpg)
str(mpg)

ggplot(mpg) +
  geom_point(aes(x= displ, y= hwy)) +
  facet_wrap(~class)



ggplot(mpg) +
  geom_point(aes(x= displ, y= hwy)) +
  facet_wrap(~cyl)



ggplot(mpg) +
  geom_point(aes(x= displ, y= hwy)) +
  facet_wrap(~hwy)

ggplot(mpg,aes(x= displ, y= hwy)) +
  geom_point() + geom_smooth()

#wigliness of the line is controlled by the span parameter

ggplot(mpg,aes(x= displ, y= hwy)) +
  geom_point() + geom_smooth(span = 0.2)

ggplot(mpg,aes(x= displ, y= hwy)) +
  geom_point() + geom_smooth(span = 1)

library("mgcv")
#if data set is huge then Loess does not work well ie span parameter fails so we use mgcv

ggplot(mpg,aes(x= displ, y= hwy)) +
  geom_point() + geom_smooth(method = "gam", formula = y~s(x))

#gives best linear fit
ggplot(mpg,aes(x= displ, y= hwy)) +
  geom_point() + geom_smooth(method = "lm")

ggplot(mpg,aes(x= drv, y= hwy)) +
  geom_jitter()

ggplot(mpg,aes(x= drv, y= hwy)) +
  geom_boxplot()

ggplot(mpg,aes(x= drv, y= hwy)) +
  geom_violin()

ggplot(mpg,aes(x = hwy)) +
  geom_freqpoly(color = "red", fill = "red")

ggplot(mpg,aes(x = hwy)) +
  geom_histogram()

head(diamonds)
ggplot(diamonds,aes(x = price)) +
  geom_freqpoly(aes(color = cut, binwidth = 50))

