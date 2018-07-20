loans <- read.csv("loans.csv")
loans_2 <- read.csv("loans_imputed.csv")
table(loans$not.fully.paid)

loans_complete <- loans[complete.cases(loans),]
loans_incomplete <- loans[!complete.cases(loans),]

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

loans <- loans_2

set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

model1 <- glm(not.fully.paid ~ ., data=train, family="binomial")
summary(model1)

predicted.risk <- predict(model1, newdata=test, type="response")
test$predicted.risk <- predicted.risk

table(test$not.fully.paid, test$predicted.risk >= 0.5)

library(ROCR)
ROCRpred = prediction(test$predicted.risk , test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

model_2 <- glm(not.fully.paid ~ int.rate, data=train, family = binomial)
prediction_2 <- predict(model_2, newdata=test, type="response")
table(test$not.fully.paid, prediction_2 >= 0.5)

library(ROCR)
ROCRpred = prediction(prediction_2 , test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)






