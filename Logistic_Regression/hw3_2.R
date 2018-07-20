parole <- read.csv("parole.csv")

str(parole)
summary(parole)

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)
summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

model_1 <-  glm(violator ~ ., data=train, family="binomial")
summary(model_1)

predict_1 <- predict(model_1, newdata=test, type="response")
max(predict_1)

table(test$violator, predict_1 >= 0.5)

library(ROCR)
ROCRpred = prediction(predict_1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)


