install.packages("tm", dependencies= TRUE)
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
