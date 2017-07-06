clinical_trial <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/clinical_trial.csv", stringsAsFactors = FALSE)
str(clinical_trial)
summary(clinical_trial)
nchar(clinical_trial[which.max(length(clinical_trial$abstract))])

nchar(clinical_trial$abstract[which.max(length(clinical_trial$abstract))])
which.max(length(clinical_trial$abstract))
nchar(clinical_trial$abstract)
which.max(length(clinical_trial$abstract))
nchar(clinical_trial$abstract[which.max(length(clinical_trial$abstract))])
max(nchar(clinical_trial$abstract))

table(nchar(clinical_trial$abstract))
clinical_trial$title[which.min(nchar(clinical_trial$title))]

library(tm)
corpusTitle <- Corpus(VectorSource(clinical_trial$title))
corpusAbstract <- Corpus(VectorSource(clinical_trial$abstract))
corpusTitle <- tm_map(corpusTitle, tolower)
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusTitle
dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dim(dtmTitle)

corpusAbstract <- tm_map(corpusAbstract, tolower)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, stemDocument)
corpusAbstract
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
dim(dtmAbstract)

which.max(colSums(dtmAbstract))

colnames(dtmTitle) <-  paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <-  paste0("A", colnames(dtmAbstract))

dtm <-  cbind(dtmTitle, dtmAbstract)
dtm$trial <- clinical_trial$trial
dim(dtm)

library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, split == TRUE)
test <- subset(dtm, split == FALSE)
table(test$trial)

library(rpart)
library(rpart.plot)

trialCart <- rpart(trial~., data = train, method = "class")
trialCartPred <- predict(trialCart, newdata = test, type ="class")
prp(trialCart)

table(test$trial, trialCartPred)

library(ROCR)

pred <- predict(trialCart, newdata = test)
ROCRPr <- prediction(pred[,2], test$trial)
performance(ROCRPr, "auc")@y.values
