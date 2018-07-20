library(ggplot2)
library(maps)
library(ggmap)

statesMap <- map_data("state")

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
          geom_polygon(fill = "white", color = "red")

polling <- read.csv("PollingImputed.csv")

Train <- subset(polling, Year < 2012 )
Test <- subset(polling, Year == 2012)

mod2 <- glm(Republican ~ SurveyUSA + DiffCount, data=Train, family="binomial")
TestPrediction <- predict(mod2, newdata=Test, type="response")

TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)

predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
          geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
          geom_polygon(color = "black") + 
          scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
          geom_polygon(color = "black") + 
          scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ 
          geom_polygon(color = "black", linetype=3, size=2, alpha = 0.5) + 
          scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")



