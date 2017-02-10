library(tm)
cname <- file.path("C:","Users","admin","Documents","AssignmentCorpus")
dir(cname)
docs <- VCorpus(DirSource(cname))
summary(docs)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs,removeWords,stopwords(kind = "en"))
docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs,stripWhitespace)
inspect(docs[5])
docs <- tm_map(docs,content_transformer(PlainTextDocument))
docs[3]
dtm <- DocumentTermMatrix(docs)
inspect(dtm[1:5,1:20])
dtms <- removeSparseTerms(dtm,0.5)
inspect(dtms)


library(wordcloud)
freqWord = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freqWord), freqWord[,1], max.words=50, colors = brewer.pal(5, "Dark2"))



d <- dist(t(dtms), method="manhattan")
fit <- hclust(d=d, method="complete")
fit
plot(fit)

d <- dist(dtms, method="manhattan")
fit1 <- hclust(d=d, method="complete")
fit1
plot(fit1)
#k_means


clus.data <- dtms
wcss <- vector()
for(i in 1:10){
  wcss[i] <- sum(kmeans(clus.data,i)$withinss)
}
plot(1:10, wcss, type ="b",)

#Elbow method shows 4 clusters

Kmeans <- kmeans(clus.data,4,iter.max = 500,nstart = 30)
summary(Kmeans)

