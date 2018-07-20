data <- read.csv("gerber.csv")

data.h <- subset(data, hawthorne == 1)
data.c <- subset(data, civicduty == 1)
data.s <- subset(data, self == 1)
data.n <- subset(data, neighbors == 1)
table(data.h$voting)/nrow(data.h)

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

model1 <- glm(voting ~ civicduty + hawthorne + self + neighbors, data=data, family=binomial)
summary(model1)
prediction1 <- predict(model1, type='response')
table(data$voting, prediction1 >= 0.5)

library(ROCR)
ROCRpred = prediction(prediction1, data$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=data)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=data, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=data, cp=0.0)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ control, data=data, cp=0.0)
prp(CARTmodel4, digits = 6)
CARTmodel5 = rpart(voting ~ control + sex, data=data, cp=0.0)
prp(CARTmodel5)

model1 <- glm(voting ~ control + sex, data=data, family=binomial)
summary(model1)
prediction1 <- predict(model1, type='response')
table(data$voting, prediction1 >= 0.5)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model1, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=data, family="binomial")

predict(LogModel2, newdata=Possibilities, type="response")




