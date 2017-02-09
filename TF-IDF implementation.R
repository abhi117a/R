#install.packages("tm", dependencies= TRUE)
library(tm)
cname <- file.path("C:", "Users","admin","Documents", "Lecture-9-Data")
cname
dir(cname)
docs <- VCorpus(DirSource(cname))
docs <- tm_map(docs, removePunctuation)
inspect(docs[5])
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs,removeWords,stopwords(kind="en"))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(PlainTextDocument))
docs[3]
dtm <- DocumentTermMatrix(docs)
dtm
inspect(dtm[1:5,1:20])
dtms <- removeSparseTerms(dtm, 0.1)
inspect(dtms)
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 20)
tail(table(freq), 20)
freq <- colSums(as.matrix(dtms))
freq
findFreqTerms(dtm, lowfreq=20)
wf <- data.frame(word = names(freq), freq = freq)
head(wf)

library(ggplot2)
p <- ggplot(subset(wf, freq>20), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
findAssocs(dtm, c("piano" , "cartoon"), corlimit=0.98)


library(wordcloud)
freqWord = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
wordcloud(rownames(freqWord), freqWord[,1], max.words=50, colors = brewer.pal(5, "Dark2"))
head(freqWord)


d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="ward.D2")
fit
plot(fit)



d <- dist(dtms, method="euclidian")
fit <- hclust(d=d, method="ward.D2")
fit
plot(fit)
