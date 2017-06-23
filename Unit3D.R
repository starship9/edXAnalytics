songs <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/songs.csv", stringsAsFactors = FALSE)
str(songs)
nrow(songs[songs$year==2010,])
nrow(songs[songs$artistname=="Michael Jackson",])
songs[songs$artistname=="Michael Jackson" & songs$Top10 == 1,]
class(songs$timesignature)
unique(songs$timesignature)
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

songTrain <- subset(songs, year<=2009)
songTest <- subset(songs,year>=2010)
nrow(songTrain)

nonvars <-  c("year", "songtitle", "artistname", "songID", "artistID")
songTrain <-  songTrain[ , !(names(songTrain) %in% nonvars) ]
songTest <-  songTest[ , !(names(songTest) %in% nonvars) ]
names(songTrain)

model1 <- glm(Top10~., data = songTrain, family = "binomial")
summary(model1)

cor(songTrain$loudness,songTrain$energy)

model2 <- glm(Top10~.-loudness, data = songTrain, family = "binomial")
summary(model2)
model3 <- glm(Top10~.-energy, data = songTrain, family = "binomial")
summary(model3)

songPred3 <- predict(model3, newdata = songTest, type = "response")
table(songTest$Top10, songPred3 > 0.45)
#table(songTest$Top10)
