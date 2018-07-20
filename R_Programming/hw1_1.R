mvt <- read.csv("mvtWeek1.csv")

str(mvt)
summary(mvt)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month))
which.max(table(mvt$Weekday))

mvt_arrest <- subset(mvt, Arrest == TRUE)
which.max(table(mvt_arrest$Month))

hist(mvt$Date, breaks=100)

mvt$Arrest <- as.numeric(mvt$Arrest)
data <- tapply(mvt$Arrest, mvt$Date, sum)
plot(data)

boxplot(mvt$Date ~ mvt$Arrest)

mvt$Year = format(DateConvert, "%Y") 

mvt_2001 <- subset(mvt, Year == 2001)
summary(mvt_2001)
sum(mvt_2001$Arrest)/nrow(mvt_2001)

mvt_2007 <- subset(mvt, Year == 2007)
summary(mvt_2007)
sum(mvt_2007$Arrest)/nrow(mvt_2007)

mvt_2012 <- subset(mvt, Year == 2012)
summary(mvt_2012)
sum(mvt_2012$Arrest)/nrow(mvt_2012)

sort(table(mvt$LocationDescription))

Top5 <- subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL" |
                             LocationDescription == "GAS STATION" |
                             LocationDescription == "ALLEY"|
                             LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|
                             LocationDescription == "STREET")

Top5$LocationDescription = factor(Top5$LocationDescription)

a <- table(Top5$LocationDescription, Top5$Arrest)

Top5_arrests <- subset(Top5, Arrest == TRUE)
table(Top5_arrests$LocationDescription, Top5_arrests$Weekday)

table(Top5$LocationDescription, Top5$Weekday)








