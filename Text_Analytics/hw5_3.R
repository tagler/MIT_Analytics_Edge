library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(ROCR)

emails <- read.csv("emails.csv", stringsAsFactors=FALSE)

which.min(nchar(emails$text))

corpus <- Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)                                                       
corpus = tm_map(corpus, removePunctuation)                                                       
corpus = tm_map(corpus, removeWords, stopwords("english"))                                                      
corpus = tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)

emailsSparse <- removeSparseTerms(dtm, 0.95)
emailsSparse <- as.data.frame(as.matrix(emailsSparse))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

colSums(emailsSparse) 
which.max(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

sort(colSums(subset(emailsSparse, spam == 0)))
sort(colSums(subset(emailsSparse, spam == 1)))

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
split <- sample.split(emailsSparse$spam, SplitRatio = 0.7)
train <- subset(emailsSparse, split==TRUE)
test <- subset(emailsSparse, split==FALSE)

spamLog <- glm(spam ~ ., data=train, family="binomial")
log_p <- predict(spamLog, newdata=train, type="response")
table(train$spam, log_p >= 0.5)
table(log_p  < 0.00001)
table(log_p  > 0.99999)
table(log_p  >= 0.00001 & log_p  <= 0.99999)
predROCR = prediction(log_p, train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

log_t <- predict(spamLog, newdata=test, type="response")
table(test$spam, log_t >= 0.5)
predROCR = prediction(log_t, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

spamCART <- rpart(spam ~ ., data=train, method="class")
cart_p <- predict(spamCART, newdata=train)
table(train$spam, cart_p[,2] >= 0.5)
prp(spamCART)
predROCR = prediction(cart_p[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

cart_t <- predict(spamCART, newdata=test)
table(test$spam, cart_t[,2] >= 0.5)
predROCR = prediction(cart_t[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

set.seed(123)
spamRF <- randomForest(spam ~ ., data=train)
rf_p <- predict(spamRF, newdata=train, type="prob")
table(train$spam, rf_p[,2] >= 0.5)
predROCR = prediction(rf_p[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

rf_t <- predict(spamRF, newdata=test, type="prob")
table(test$spam, rf_t[,2] >= 0.5)
predROCR = prediction(rf_t[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values






