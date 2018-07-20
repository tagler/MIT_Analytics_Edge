tweets <- read.csv("tweets.csv",  stringsAsFactors=FALSE )

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

library(wordcloud)

colnames(allTweets)
colSums(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

library(RColorBrewer)



