library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(tm)

tweets <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/tweets.csv",stringsAsFactors = FALSE)
str(tweets)

tweetCorpus <- Corpus(VectorSource(tweets$Tweet))
tweetCorpus <- tm_map(tweetCorpus, tolower)
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(tweetCorpus)
allTweets <- as.data.frame(as.matrix(dtm))
str(allTweets)
dim(allTweets)

wordcloud(colnames(allTweets),freq = colSums(allTweets),scale = c(2,0.25))

tweetCorpus2 <- Corpus(VectorSource(tweets$Tweet))
tweetCorpus2 <- tm_map(tweetCorpus2, tolower)
tweetCorpus2 <- tm_map(tweetCorpus2, removePunctuation)
tweetCorpus2 <- tm_map(tweetCorpus2, removeWords, c(stopwords("english"),"apple"))
dtm2 <- DocumentTermMatrix(tweetCorpus2)
allTweets2 <- as.data.frame(as.matrix(dtm2))
str(allTweets2)
dim(allTweets2)

wordcloud(colnames(allTweets2),freq = colSums(allTweets2),scale = c(2,0.25))
