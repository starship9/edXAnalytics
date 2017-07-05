tweets <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/tweets.csv",stringsAsFactors = FALSE)
library(tm)

tweets$negative <- as.factor(tweets$Avg<=-1)
head(tweets$negative)
table(tweets$negative)
library(SnowballC)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus[1]

#lower case
corpus <- tm_map(corpus, tolower)
#corpus <-  tm_map(corpus, PlainTextDocument)
head(corpus)

#remove punctuation
corpus <- tm_map(corpus, removePunctuation)

#stopwords
stopwords("english")
corpus <- tm_map(corpus, removeWords, c("apple",stopwords("english")))

#stemming
corpus <- tm_map(corpus, stemDocument)

#DTM
library(SnowballC)
#corpus <- Corpus(VectorSource(corpus))
frequencies <- DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq = 20)

#sparsity matrix
sparse <- removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse <- as.data.frame(as.matrix(sparse))

#proper naming!
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$negative <- tweets$negative

library(caTools)
set.seed(123)

split <- sample.split(tweetsSparse$negative, SplitRatio = 0.7)
training <- subset(tweetsSparse, split == TRUE)
testing <- subset(tweetsSparse, split == FALSE)

library(rpart)
library(rpart.plot)

tweetCart <- rpart(negative ~., data = training, method = "class")
prp(tweetCart)

predictCart <- predict(tweetCart, newdata = testing, type = "class")
table(testing$negative, predictCart)

library(randomForest)
set.seed(123)

tweetRF <- randomForest(negative ~., data = training)
predictRF <- predict(tweetRF, newdata = testing)
table(testing$negative, predictRF)

tweetLogReg <- glm(negative~., data = training, family = "binomial")
predictLogReg <- predict(tweetLogReg, newdata = testing, type = "response")
