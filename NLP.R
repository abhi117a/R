#NLP

#install.packages('tm',repos='http://cran.us.r-project.org')
#install.packages("tm",dependencies=TRUE)
#install.packages('twitteR',repos='http://cran.us.r-project.org')
#install.packages('wordcloud',repos='http://cran.us.r-project.org')
#install.packages('RColorBrewer',repos='http://cran.us.r-project.org')

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

#Put your 
#ckey <- Consumer Key (API Key)
#skey <- Consumer Secret (API Secret)
#token <- Access Token
#secToken <- Access Token Secret

#Connect to Twitter
setup_twitter_oauth(ckey,skey,token,secToken)

#Search
search_soccer <- searchTwitter("soccer",n=100, lang = "en")

soccer_text <- sapply(search_soccer, function(x) x$getText())

#Cleaning The Data

soccer_text  <- iconv(soccer_text,"UTF-8","ASCII")

soccer_corpus <- Corpus(vectorSource(soccer_text))

#Document Term Matrix

term_doc_matrix <- TermDocumentMatrix(soccer_corpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords = c("soccer","http", stopwords("english")),
                                                     removeNumbers = TRUE,tolower = TRUE))


term_doc_matrix <- as.matrix(term_doc_matrix)



word_freq <- sort(rowSums(term_doc_matrix), decreasing = T)
dm <- data.frame(word = names(word_freq),freq = word_freq)


word_cloud <- wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))







