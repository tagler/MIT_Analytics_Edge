clinical <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(ROCR)

max(nchar(clinical$abstract))
table(nchar(clinical$abstract) == 0)
index <- which.min(nchar(clinical$title))
clinical$title[index]

corpusTitle <- Corpus(VectorSource(clinical$title))
corpusAbstract <- Corpus(VectorSource(clinical$abstract))

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)                                                       
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)                                                       
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))                                                      
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

sparseTitle <- removeSparseTerms(dtmTitle, 0.95)
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.95)

dtmTitle <- as.data.frame(as.matrix(sparseTitle))
dtmAbstract <- as.data.frame(as.matrix(sparseAbstract))

which.max(colSums(dtmAbstract) )
                                                                  
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinical$trial

library(caTools)
set.seed(144)
split <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, split==TRUE)
test <- subset(dtm, split==FALSE)

table(train$trial)

trialCART <- rpart(trial ~ ., data = train, method="class")
prp(trialCART)

predictions_train <- predict(trialCART, newdata=train)
predictions_train <- predictions_train[,2]
table(train$trial, predictions_train >= 0.5)

preditions_test <- predict(trialCART, newdata=test)
preditions_test  <- preditions_test[,2]
table(test$trial, preditions_test  >= 0.5)

predROCR = prediction(preditions_test, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values


          
          
          
          










