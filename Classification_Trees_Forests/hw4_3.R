census <- read.csv("census.csv")

library(caTools)
set.seed(2000)
spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

model1 <- glm(over50k ~ ., data=train, family=binomial)
summary(model1)
prediction1 <- predict(model1, newdata=test, type='response')
table(test$over50k, prediction1 >= 0.5)

library(ROCR)
ROCRpred = prediction(prediction1, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~ . , data=train, method="class")
prp(CARTmodel)
prediction1 <- predict(CARTmodel, newdata=test, type="class")
table(test$over50k, prediction1)

confusionMatrix(test$over50k, prediction1)

prediction1 <- predict(CARTmodel, newdata=test)
library(ROCR)
ROCRpred = prediction(prediction1[,2], test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
library(randomForest)
set.seed(1)
rf = randomForest(over50k ~ . , data=trainSmall)
prediction2 <- predict(rf, newdata=test)
table(test$over50k, prediction2)

vu = varUsed(rf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(rf$forest$xlevels[vusorted$ix]))
varImpPlot(rf)

library(caret)
library(e1071)
set.seed(2)
numFolds = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid )
cp =  0.002
newmodel = rpart(over50k ~ . , data = train, method="class", cp = 0.002)
PredictCV = predict(newmodel, newdata = test, type = "class")
table(test$over50k, PredictCV)
prp(newmodel)


