# Kaggle - New York Times Blog Article Popularity

## About

This Kaggle competition, part of MITx - The Analytics Edge (15.071x), investigated predicting the popularity of New York Times blog articles. 

Newspapers and online news aggregators like Google News need to understand which news articles will be the most popular, so that they can prioritize the order in which stories appear. In this competition, you will predict the popularity of a set of New York Times blog articles from the time period September 2014-December 2014.

## Data 

Website: https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015    
Data Source: https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015/data

## Results 

The test set score was 0.90451, which resulted in a rank of 186 out of 2,923 entries (top 7%).

## Source Code


```r
library(timeDate)
library(tm)
library(caret)
library(randomForest)
library(caTools)
library(ROCR)
library(pROC)
library(glmnet)
library(mice)
```


```r
# LOAD DATA 

# train and test sets 
rawTrain <- read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE, na.strings = "")
rawTest <- read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE, na.strings = "")

# convert popular field to factor (Yes/No)
rawTest$Popular <- NA
popular_values <- as.factor(rawTrain$Popular)
levels(popular_values) <- c("NO","YES")
rawTrain$Popular <- popular_values

# combine data
TrainTest <- rbind(rawTrain, rawTest)
```


```r
# FORMAT DATES 

# POSIXlt  conversion 
TrainTest$PubDate <- strptime(TrainTest$PubDate, "%Y-%m-%d %H:%M:%S")

# add day of the week column
TrainTest$PubDate_Weekday <- as.factor(TrainTest$PubDate$wday)

# add hour of day column
TrainTest$PubDate_Hour <- as.factor(TrainTest$PubDate$hour)
```


```r
# LOG TRANSFORMATIONS 

# log transformation, skewed data
TrainTest$WordCount_Log <- log( 1 + TrainTest$WordCount )
```


```r
# TEXT TRANSFORMATIONS

# mark questions
TrainTest$Headline_question <- as.factor(grepl("\\<where\\>|\\<who\\>|\\<when\\>|\\<why\\>|\\<how\\>|\\?", 
    TrainTest$Headline, ignore.case=TRUE))
TrainTest$Abstract_question <- as.factor(grepl("\\<where\\>|\\<who\\>|\\<when\\>|\\<why\\>|\\<how\\>|\\?", 
    TrainTest$Abstract, ignore.case=TRUE))

# headline text anlaysis
CorpusHeadline <- Corpus(VectorSource(TrainTest$Headline))
CorpusHeadline <- tm_map(CorpusHeadline, tolower)
CorpusHeadline <- tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline <- tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline <- tm_map(CorpusHeadline, removeNumbers)
CorpusHeadline <- tm_map(CorpusHeadline, removeWords, stopwords("english"))
dtmHeadline <- DocumentTermMatrix(CorpusHeadline)
dtmHeadline <- weightSMART(dtmHeadline, spec = "ntc")
sparseHeadline <- removeSparseTerms(dtmHeadline, 0.989)
HeadlineWords <- as.data.frame(as.matrix(sparseHeadline))
colnames(HeadlineWords) <- make.names(colnames(HeadlineWords))
colnames(HeadlineWords) <- paste("H", colnames(HeadlineWords), sep = "_")

# abstract text analysis 
CorpusAbstract <- Corpus(VectorSource(TrainTest$Abstract))
CorpusAbstract <- tm_map(CorpusAbstract, tolower)
CorpusAbstract <- tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract <- tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract <- tm_map(CorpusAbstract, removeNumbers)
CorpusAbstract <- tm_map(CorpusAbstract, removeWords, stopwords("english"))
dtmAbstract <- DocumentTermMatrix(CorpusAbstract)
dtmAbstract <- weightSMART(dtmAbstract, spec = "ntc") 
sparseAbstract <- removeSparseTerms(dtmAbstract, 0.975)
AbstractWords <- as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) <- make.names(colnames(AbstractWords))
colnames(AbstractWords) <- paste("A", colnames(AbstractWords), sep = "_")
```


```r
# ORGANIZE DATA 

# combine data
TrainTestWords <- cbind(TrainTest, HeadlineWords, AbstractWords)

# remove columns
TrainTestWords$Headline <- NULL
TrainTestWords$Snippet <- NULL
TrainTestWords$Abstract <- NULL
TrainTestWords$PubDate <- NULL
TrainTestWords$UniqueID <- NULL
TrainTestWords$WordCount <- NULL

# seperate out dependent variable 
POPULAR_VALUES <- TrainTestWords$Popular
TrainTestWords$Popular <- NULL
```


```r
# MISSING DATA 

# best-category column 
TrainTestWords$Best_Category <- NA
for (i in 1:nrow(TrainTestWords)) {
          if ( !is.na(TrainTestWords$SubsectionName[i]) ) { 
              TrainTestWords$Best_Category[i] <- TrainTestWords$SubsectionName[i]
          } else if ( !is.na(TrainTestWords$SectionName[i]) ) { 
              TrainTestWords$Best_Category[i] <- TrainTestWords$SectionName[i]
          } else if ( !is.na(TrainTestWords$NewsDesk[i]) ) { 
              TrainTestWords$Best_Category[i] <- TrainTestWords$NewsDesk[i]
          } else { TrainTestWords$Best_Category[i] <- NA } }

# other category to replace NA values
TrainTestWords$Best_Category[is.na(TrainTestWords$Best_Category)] <- "OTHER"
TrainTestWords$NewsDesk[is.na(TrainTestWords$NewsDesk)] <- "OTHER"
TrainTestWords$SectionName[is.na(TrainTestWords$SectionName)] <- "OTHER"
TrainTestWords$SubsectionName[is.na(TrainTestWords$SubsectionName)] <- "OTHER"

# factors
TrainTestWords$Best_Category <- as.factor(TrainTestWords$Best_Category)
TrainTestWords$NewsDesk <- as.factor(TrainTestWords$NewsDesk)
TrainTestWords$SectionName <- as.factor(TrainTestWords$SectionName)
TrainTestWords$SubsectionName <- as.factor(TrainTestWords$SubsectionName)

# inpute via mice
TrainTestWords$NewsDesk <- NULL
TrainTestWords$SectionName <- NULL
TrainTestWords$SubsectionName <- NULL
impute_data <- mice(TrainTestWords, MaxNWts = 5000)
impute_data_frame <- complete(impute_data)
TrainTestEdit <- impute_data_frame
TrainTestEdit <- TrainTestWords
TrainTestEdit$Popular <- POPULAR_VALUES 
```


```r
# ORGANIZE DATA  

# split data
ModTrain <- TrainTestEdit[1:6532,]
kaggle_test <- TrainTestEdit[6533:8402,]

# make new training and testing data sets
set.seed(5150)
spl <- sample.split(ModTrain$Popular, SplitRatio = 0.8)
my_train <- subset(ModTrain, spl==TRUE)
my_test <- subset(ModTrain, spl==FALSE)
```


```r
# LOG MODELS 

# log regression 1
model_glm_1 <- glm(Popular ~ Best_Category + PubDate_Hour + WordCount_Log, 
                 data = my_train, family = binomial)
model_glm_prediction_1 <- predict(model_glm_1, newdata = my_test, type = "response")
prediction_1 <- as.factor(model_glm_prediction_1 >= 0.5)
levels(prediction_1) <- c("NO","YES")
confusionMatrix( my_test$Popular, prediction_1)
ROCRpred_1 = prediction(model_glm_prediction_1, my_test$Popular)
as.numeric(performance(ROCRpred_1, "auc")@y.values)

# log regression 2
model_glm_2 <- glm(Popular ~ Best_Category + PubDate_Weekday + WordCount_Log + Headline_question + 
                       today + A_times + A_year, data = my_train, family = binomial)
model_glm_prediction_2 <- predict(model_glm_2, newdata = my_test, type = "response")
prediction_2 <- as.factor(model_glm_prediction_2 >= 0.5)
levels(prediction_2) <- c("NO","YES")
confusionMatrix( my_test$Popular, prediction_2)
ROCRpred_2 = prediction(model_glm_prediction_2, my_test$Popular)
as.numeric(performance(ROCRpred_2, "auc")@y.values)

# log regression 3
model_glm_3 <- glm(Popular ~ Best_Category + WordCount_Log + PubDate_Weekday + Headline_question + 
                       Abstract_question + H_today, data = my_train, family = binomial)
model_glm_prediction_3 <- predict(model_glm_3, newdata = my_test, type = "response")
prediction_3 <- as.factor(model_glm_prediction_3 >= 0.5)
levels(prediction_3) <- c("NO","YES")
confusionMatrix( my_test$Popular, prediction_3)
ROCRpred_3 = prediction(model_glm_prediction_3, my_test$Popular)
as.numeric(performance(ROCRpred_3, "auc")@y.values)

# log regression, 0.9412 AUC <------ best 
model_glm_2 <- glm(Popular ~ Category + WordCountLog + PubDate_Weekday + Headline_question + 
                       Abstract_question + H_today, data = my_train, family = binomial)
model_glm_prediction <- predict(model_glm_2, newdata = my_test, type = "response")
confusionMatrix( as.logical(my_test$Popular), (model_glm_prediction >= 0.5) )
ROCRpred = prediction(model_glm_prediction, as.logical(my_test$Popular))
as.numeric(performance(ROCRpred, "auc")@y.values)

# glmet regularization 
x <- data.matrix(my_train[,2:(length(my_train))])
y <- my_train[,1]
model_glmnet_1 <- glmnet(x, y, family = "binomial")
plot(model_glmnet_1 , xvar = "dev", label = TRUE)
cvfit <- cv.glmnet(x, y, family = "binomial" )
coef(cvfit, s = "lambda.min")
newx <- data.matrix(my_test[,2:(length(my_train))])
predictions <- predict(cvfit, newx, s = "lambda.min", type = "response")
confusionMatrix( as.logical(my_test$Popular), (predictions >= 0.5) )
ROCRpred = prediction(predictions, as.logical(my_test$Popular))
as.numeric(performance(ROCRpred, "auc")@y.values)
```


```r
# RANDOM FOREST MODELS 

# random forest - basic variables
model_rf <- randomForest(as.factor(Popular) ~ Category + WordCountLog + PubDate_Weekday + PubDate_Hour, 
                             data=my_train)
model_rf_prediction <- predict(model_rf, newdata=my_test, type="prob")[,2]
confusionMatrix( my_test$Popular , (as.logical( (model_rf_prediction >= 0.5) )+0)  )
ROCRpred = prediction(model_rf_prediction, my_test$Popular )
as.numeric(performance(ROCRpred, "auc")@y.values)

# random forest - basic + top text variables
model_rf_2 <- randomForest(as.factor(Popular) ~ Category + WordCountLog + PubDate_Weekday + 
                               Headline_question + Abstract_question + H_today, data=my_train,
                               ntree=5000, nodesize=10)
model_rf_prediction <- predict(model_rf_2, newdata=my_test, type="prob")[,2]
confusionMatrix( my_test$Popular , (as.logical( (model_rf_prediction >= 0.5) )+0)  )
ROCRpred = prediction(model_rf_prediction, my_test$Popular )
as.numeric(performance(ROCRpred, "auc")@y.values)

# random forest - all variables 
model_rf_3 <- randomForest(as.factor(Popular) ~ ., data=my_train)
model_rf_prediction <- predict(model_rf_3, newdata=my_test, type="prob")[,2]
confusionMatrix( my_test$Popular , (as.logical( (model_rf_prediction >= 0.5) )+0)  )
ROCRpred = prediction(model_rf_prediction, my_test$Popular )
as.numeric(performance(ROCRpred, "auc")@y.values)

# re-level
y <- as.factor(my_train$Popular)
levels(y) <- c("NO","YES")

# random forest via caret
fitControl <- trainControl(classProbs = TRUE)
model_rf_3 <- train( y ~ Category + WordCountLog + PubDate_Weekday + Headline_question + 
                         Abstract_question + H_today, data=my_train, method="rf",
                         nodesize=5, ntree=500, metric="ROC", trControl=fitControl)
model_rf_prediction <- predict(model_rf_3, newdata=my_test, type="prob")
confusionMatrix( my_test$Popular , (as.logical( (model_rf_prediction >= 0.5) )+0)  )
ROCRpred = prediction(model_rf_prediction, my_test$Popular )
as.numeric(performance(ROCRpred, "auc")@y.values)
```


```r
# OUTPUT RESULTS 

# logistic regression output code
kaggle_prediction <- predict(model_glm_2 , newdata=kaggle_test, type = "response")
kaggle_submission <- data.frame(UniqueID = rawTest$UniqueID, Probability1 = kaggle_prediction )
write.csv(kaggle_submission, "results.csv", row.names=FALSE)

# random forest output code 
kaggle_prediction <- predict(model_rf_2  , newdata=kaggle_test, type="prob")[,2]
kaggle_submission <- data.frame(UniqueID = rawTest$UniqueID, Probability1 = kaggle_prediction )
write.csv(kaggle_submission, "results.csv", row.names=FALSE)
```
