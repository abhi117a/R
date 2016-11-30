
#Commandline input
args = commandArgs(((trailingOnly=TRUE)))
#Throw error if there are not exactly two arguments
if(length(args)!=3){
  stop("The program requires exactly three arguments...", call. = FALSE)
}
library(ggplot2)
library(jpeg)

img1 = readJPEG("image4.jpg")
imgDm1 = dim(img1)

img2 = readJPEG(args[2])
imgDm2 = dim(img2)

img3 = readJPEG(args[3])
imgDm3 = dim(img3)


imgRGB1 = data.frame(
  x1 = rep(1:imgDm1[2], each = imgDm1[1]),
  y1 = rep(imgDm1[1]:1, imgDm1[2]),
  R1 = as.vector(img1[,,1]),
  G1 = as.vector(img1[,,2]),
  B1 = as.vector(img1[,,3])
)


imgRGB2 = data.frame(
  x2 = rep(1:imgDm2[2], each = imgDm2[1]),
  y2 = rep(imgDm2[1]:1, imgDm2[2]),
  R2 = as.vector(img2[,,1]),
  G2 = as.vector(img2[,,2]),
  B2 = as.vector(img2[,,3])
)

imgRGB3 = data.frame(
  x3 = rep(1:imgDm3[2], each = imgDm3[1]),
  y3 = rep(imgDm3[1]:1, imgDm3[2]),
  R3 = as.vector(img3[,,1]),
  G3 = as.vector(img3[,,2]),
  B3 = as.vector(img3[,,3])
)

#for image 1

kClusters <- 3
kMeans1 <- kmeans(imgRGB1[, c("R1", "G1", "B1")], centers = kClusters)
kColours1 <- rgb(kMeans1$centers[kMeans1$cluster,])
ggplot(data = imgRGB1, aes(x = x1, y = y1)) + geom_point(colour = kColours1)
ggsave("output_image1.jpg", last_plot())


#for image 2
kMeans2 <- kmeans(imgRGB2[, c("R2", "G2", "B2")], centers = kClusters)
kColours2 <- rgb(kMeans2$centers[kMeans2$cluster,])

ggplot(data = imgRGB2, aes(x = x2, y = y2)) + geom_point(colour = kColours2)
ggsave("output_image2.jpg", last_plot())

#for image 3
kMeans3 <- kmeans(imgRGB3[, c("R3", "G3", "B3")], centers = kClusters)
kColours3 <- rgb(kMeans3$centers[kMeans3$cluster,])

ggplot(data = imgRGB3, aes(x = x3, y = y3)) + geom_point(colour = kColours3)
ggsave("output_image3.jpg", last_plot())