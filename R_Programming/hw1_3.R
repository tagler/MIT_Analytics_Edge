CPS <- read.csv("CPSData.csv")

sort(table(CPS$Region))

summary(CPS)

data_hispanic <- subset(CPS, Hispanic == 1)
summary(data_dispanic)

is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
      
table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))

sort( tapply( is.na(CPS$MetroAreaCode) , CPS$State, mean) )

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

sort(table(CPS$MetroArea))

sort( tapply( (CPS$Hispanic), CPS$MetroArea, mean) )

sort( tapply( CPS$Race == "Asian", CPS$MetroArea, mean) )

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

CPS2 <- subset(CPS, !is.na(CPS$Country) )
CPS3 <- subset(CPS2, MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
summary(CPS3)

sort( tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE) )
sort( tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE) )
sort( tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE) )

