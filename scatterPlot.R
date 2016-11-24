#ScatterPlots

df <- mtcars
head(df)

library(ggplot2)
g <- ggplot(df)
g+ geom_point(aes(x=df$wt, y = df$mpg, color = factor(df$cyl)))


g+ geom_point(aes(x=df$wt, y = df$mpg, size = factor(df$cyl), color = df$hp)) + scale_color_gradient(low = "green", high = "red")
