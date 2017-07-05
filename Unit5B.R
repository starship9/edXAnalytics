energy_bids <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/energy_bids.csv", stringsAsFactors = FALSE)
str(energy_bids)
energy_bids$email[1]
energy_bids$responsive[1]
table(energy_bids$responsive)

library(tm)
corpus <- Corpus(VectorSource(energy_bids$email))
corpus[[1]]
corpus

#preprocessing
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]

dtm <- DocumentTermMatrix(corpus)
dtm

dtm <- removeSparseTerms(dtm, 0.97)
dtm

labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms$responsive <- energy_bids$responsive
str(labeledTerms)

library(caTools)
set.seed(144)

split <- sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train <- subset(labeledTerms, split == TRUE)
test <- subset(labeledTerms, split == FALSE)

library(rpart)
library(rpart.plot)

emailCart <- rpart(responsive ~., data = train, method = "class")
prp(emailCart)

pred <- predict(emailCart, newdata = test)
pred[1:10,]

pred.prob <- pred[,2]
table(test$responsive, pred.prob > 0.5)
table(test$responsive)

library(ROCR)
predROCR <- prediction(pred.prob, test$responsive)
perfROCR <- performance(predROCR, "tpr","fpr")
plot(perfROCR, colorize = TRUE)
performance(predROCR, "auc")@y.values
