data <- read.csv("climate_change.csv")

training_set <- subset(data, Year < 2007)
testing_set <- subset(data, Year > 2006)

model_1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training_set)
summary(model_1)

cor(training_set)

model_2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=training_set)
summary(model_2)

model_3 <- step(model_1)
summary(model_3)

predict_1 <- predict(model_3, newdata=testing_set)
real_values <- testing_set$Temp

SSE = sum((predict_1 - testing_set$Temp)^2)
SST = sum((mean(training_set$Temp) - testing_set$Temp)^2)
R2 = 1 - SSE/SST
R2
