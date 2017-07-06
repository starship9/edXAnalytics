emails <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/emails.csv", stringsAsFactors = FALSE)
str(emails)
table(emails$spam)
nchar(emails$text[which.max(length(emails$text))])
max(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)
corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
dtm

spdtm <- removeSparseTerms(dtm, 0.95)
spdtm

emailSparse <- as.data.frame(as.matrix(spdtm))
names(emailSparse)
#make.names(emailSparse)
names(emailSparse) <- make.names(names(emailSparse), unique = TRUE)
names(emailSparse)

summary(sort(colSums(emailSparse)>5000))

emailSparse$spam <- emails$spam
str(emailSparse)

sum(colSums(emailSparse) > 5000)
sort(colSums(subset(emailSparse, spam == 0)))
sort(colSums(subset(emailSparse,spam == 1)))

emailSparse$spam <-  as.factor(emailSparse$spam)

library(caTools)
set.seed(123)
split <- sample.split(emailSparse$spam, SplitRatio = 0.7)
train <- subset(emailSparse, split == TRUE)
test <- subset(emailSparse, split == FALSE)

spamLog <- glm(spam~., data = train, family = "binomial")
library(rpart)
library(rpart.plot)

spamCart <- rpart(spam~., data = train, method = "class")

prp(spamCart)

library(randomForest)

set.seed(123)
spamRF <- randomForest(spam~., data = train)

spamLogPred <- predict(spamLog, type = "response",newdata = test)
spamCartPred <- predict(spamCart,newdata = test)
spamRFPred <- predict(spamRF, type = "prob",newdata = test)

table(spamLogPred<0.00001)
table(spamLogPred>0.99999)
table(spamLogPred>0.00001 & spamLogPred<0.99999)

summary(spamLog)

table(test$spam, spamLogPred>0.5)

library(ROCR)
logROC <- prediction(spamLogPred, test$spam)
performance(logROC, "auc")@y.values

table(test$spam, spamCartPred[,2]>0.5)

cartROC <- prediction(spamCartPred[,2], test$spam)
performance(cartROC, "auc")@y.values

table(test$spam, spamRFPred[,2]>0.5)

rfROC <- prediction(spamRFPred[,2], test$spam)
performance(rfROC, "auc")@y.values

wordCount <-  rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))

emailSparse$logWordCount <- log(wordCount)
boxplot(emailSparse$logWordCount~emailSparse$spam)

train2 <- subset(emailSparse,split==TRUE)
test2 <- subset(emailSparse, split == FALSE)

spamCart2 <- rpart(spam~., data = train2, method = "class")
set.seed(123)
spamRF2 <- randomForest(spam~., data = train2)
prp(spamCart2)

spamCartPred2 <- predict(spamCart2, newdata = test2, type = "class")
table(test2$spam, spamCartPred2)

#removed class argument for AUC
spamCartPred2 <- predict(spamCart2, newdata = test2)
cartROC2 <- prediction(spamCartPred2[,2], test2$spam)
performance(cartROC2, "auc")@y.values

spamRFPred2 <- predict(spamRF2, newdata = test2)
table(test2$spam, spamRFPred2)


rfROC2 <- prediction(spamRFPred2[,2], test2$spam)
performance(rfROC2, "auc")@y.values

#rfROC <- prediction(spamRFPred[,2], test$spam)
#performance(rfROC, "auc")@y.values
