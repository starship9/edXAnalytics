wiki <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/wiki.csv", stringsAsFactors = FALSE)
str(wiki)
table(wiki$Vandal)

wiki$Vandal <- as.factor(wiki$Vandal)
table(wiki$Vandal)

#preprocessing
corpus <- Corpus(VectorSource(wiki$Added))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtmAdded <- DocumentTermMatrix(corpus)
dtmAdded

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <-  paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
dtmRemoved
corpusRemoved

wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <-  paste("R", colnames(wordsRemoved))
dim(wordsRemoved)
str(wordsRemoved)

wikiWords <-  cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train <- subset(wikiWords, split == TRUE)
test <- subset(wikiWords, split == FALSE)

table(test$Vandal)

library(rpart)
library(rpart.plot)

vandalCart <- rpart(Vandal ~., data = train, method = "class")
prp(vandalCart)
cartPred <- predict(vandalCart, newdata = test, type = "class")
table(test$Vandal, cartPred)

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 <-  subset(wikiWords2, split==TRUE)
wikiTest2 <-  subset(wikiWords2, split==FALSE)

vandalCart2 <- rpart(Vandal ~., data = wikiTrain2, method = "class")
cart2Pred <- predict(vandalCart2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, cart2Pred)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

wikiTrain3 <- subset(wikiWords2, split == TRUE)
wikiTest3 <- subset(wikiWords2, split == FALSE)

vandalCart3 <- rpart(Vandal~., data = wikiTrain3, method = "class")
cart3Pred <- predict(vandalCart3, newdata = wikiTest3, type = "class")
table(wikiTest3$Vandal, cart3Pred)

wikiWords3 <- wikiWords2
wikiWords3$Minor <-  wiki$Minor
wikiWords3$Loggedin <-  wiki$Loggedin

wikiTrain4 <- subset(wikiWords3, split == TRUE)
wikiTest4 <- subset(wikiWords3, split == FALSE)

vandalCart4 <- rpart(Vandal~., data = wikiTrain4, method = "class")
cart4Pred <- predict(vandalCart4, newdata = wikiTest4, type = "class")
table(wikiTest4$Vandal, cart4Pred)

prp(vandalCart4)
