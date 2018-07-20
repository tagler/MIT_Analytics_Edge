FluTrain <- read.csv("FluTrain.csv")
FluTest <- read.csv("FluTest.csv")

FluTrain[which.max(FluTrain$ILI),]
FluTrain[which.max(FluTrain$Queries),]

hist(FluTrain$ILI)

plot(log(FluTrain$ILI), FluTrain$Queries)
plot(FluTrain$Queries, log(FluTrain$ILI))

model_1 <- lm( log(ILI) ~ Queries, data=FluTrain)
summary(model_1)

cor(FluTrain$ILI, FluTrain$Queries)

PredTest1 = exp(predict(model_1, newdata=FluTest))
which(FluTest$Week == "2012-03-11 - 2012-03-17")

PredTest1[11]
FluTest$ILI[11]

RE <- (FluTest$ILI[11] - PredTest1[11])/FluTest$ILI[11]

RMSE = sqrt(mean((PredTest1-FluTest$ILI)^2))

library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILI), log(FluTrain$ILILag2))

FluTrend2 <- lm( log(ILI) ~ Queries + log(ILILag2), data=FluTrain)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

RMSE = sqrt(mean((PredTest2-FluTest$ILI)^2))



