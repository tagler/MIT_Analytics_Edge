wiki <- read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

library(tm)
library(SnowballC)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)
dtmAdded <- DocumentTermMatrix(corpusAdded)
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal

library(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train <- subset(wikiWords, split==TRUE)
test <- subset(wikiWords, split==FALSE)
table(test$Vandal)

library(rpart)
library(rpart.plot)
library(caret)
wikiCART <- rpart(Vandal ~ ., data=train, method="class")
predictCART <- predict(wikiCART, newdata=test, type="class")
table(test$Vandal, predictCART)
confusionMatrix(test$Vandal, predictCART)
prp(wikiCART)

wikiWords2 <- wikiWords          
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 <- rpart(Vandal ~ ., data=wikiTrain2, method="class")
predictCART2 <- predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, predictCART2)
confusionMatrix(wikiTest2$Vandal, predictCART2)
prp(wikiCART2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)

wikiCART3 <- rpart(Vandal ~ ., data=wikiTrain3, method="class")
predictCART3 <- predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, predictCART3)
confusionMatrix(wikiTest3$Vandal, predictCART3)
prp(wikiCART3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, split==TRUE)
wikiTest4 = subset(wikiWords3, split==FALSE)

wikiCART4 <- rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictCART4 <- predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictCART4)
confusionMatrix(wikiTest4$Vandal, predictCART4)
prp(wikiCART4)





