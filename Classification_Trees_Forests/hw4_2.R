letters <- read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
spl = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(test$isB)

library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)
prediction1 <- predict(CARTb, newdata=test, type='class')
table(test$isB, prediction1)

library(randomForest)
set.seed(1000)
rf = randomForest(isB ~ . - letter, data=train)
prediction2 <- predict(rf, newdata=test)
table(test$isB, prediction2)

letters$letter = as.factor( letters$letter )
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = subset(letters, spl==TRUE)
test = subset(letters, spl==FALSE)
table(test$letter)

CART1 = rpart(letter ~ . - isB, data=train, method="class")
prp(CART1)
prediction1 <- predict(CART1, newdata=test, type='class')
table(test$letter, prediction1)

set.seed(1000)
r2 = randomForest(letter ~ . - isB, data=train)
prediction2 <- predict(r2, newdata=test)
table(test$letter, prediction2)



