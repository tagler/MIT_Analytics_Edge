stocks <- read.csv("StocksCluster.csv")

dec <- subset(stocks, stocks$PositiveDec > 0)
table(stocks$PositiveDec)
max(cor(stocks[,1:11]))
cor(stocks$ReturnJan, stocks$ReturnFeb)
colMeans(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
predicted <- predict(StocksModel, newdata=stocksTrain, type="response")
table(stocksTrain$PositiveDec, predicted >= 0.5)
predicted2 <- predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, predicted2 >= 0.5)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

colMeans(stocksTrain1)

StocksModel1 <- glm(PositiveDec ~ . , data=stocksTrain1, family=binomial)
StocksModel2 <- glm(PositiveDec ~ . , data=stocksTrain2, family=binomial)
StocksModel3 <- glm(PositiveDec ~ . , data=stocksTrain3, family=binomial)
PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type="response")
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type="response")
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type="response")
table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions >= 0.5)
