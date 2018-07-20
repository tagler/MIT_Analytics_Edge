test <- read.csv("pisa2009test.csv")
train <- read.csv("pisa2009train.csv")

male_train <- subset(train, male == 1)
female_train <- subset(train, male == 0)

mean(male_train$readingScore)
mean(female_train$readingScore)

tapply(train$readingScore, train$male, mean)

summary(train)

pisaTrain = na.omit(train)
pisaTest = na.omit(test)

table(train$raceeth)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

lmScore$residuals
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

sqrt(mean(lmScore$residuals^2))

predTest <- predict(lmScore, newdata=pisaTest)

range(predTest)

SSE = sum((predTest-pisaTest$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2))
SST = sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST
