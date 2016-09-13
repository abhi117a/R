a = matrix(c(1,2,1,1,2,2),nrow=3,ncol = 2,byrow = TRUE)
a
df = data.frame(a)
pca.eg <- prcomp(df, scale. = FALSE)
pca.eg
summary(pca.eg)