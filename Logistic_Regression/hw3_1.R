songs <- read.csv("songs.csv")

songs_2010 <- subset(songs, year == 2010)
table(songs$year)
artists <- sort(table(songs$artistname))
artists_mj <- subset(songs, songs$artistname == "Michael Jackson")
table(songs$timesignature)
songs[which.max(songs$tempo),]

SongsTrain <- subset(songs, year < 2010)
SongsTest <- subset(songs, year > 2009)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 <- glm(Top10 ~ ., data=SongsTrain, family="binomial")
summary(SongsLog1)

table(songs$tempo_confidence)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

prediction_3 <- predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, prediction_3 >= 0.45)
library(caret); library(ggplot2)
confusionMatrix(SongsTest$Top10, as.numeric(prediction_3 >= 0.45))

summary(SongsTrain)
table(SongsTest$Top10)

table(prediction_3 >= 0.45, SongsTest$Top10)
confusionMatrix(as.numeric(prediction_3 >= 0.45), SongsTest$Top10)

table(SongsTest$Top10, prediction_3 >= 0.45)
sen <- 19/(19+40)
spf <- 309/(309+5)





