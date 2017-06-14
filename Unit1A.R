library(readr)
mvtWeek1 <-
  read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/mvtWeek1.csv",
           stringsAsFactors = FALSE)
View(mvtWeek1)

nrow(mvtWeek1)
str(mvtWeek1)
max(mvtWeek1$ID)
min(mvtWeek1$Beat)
table(mvtWeek1$Arrest)
table(mvtWeek1$LocationDescription)
head(mvtWeek1$Date)
DateConvert = as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvtWeek1$Month <-  months(DateConvert)

mvtWeek1$Weekday <-  weekdays(DateConvert)
mvtWeek1$Date <- DateConvert
table(mvtWeek1$Month)
table(mvtWeek1$Weekday)

table(mvtWeek1$Month, mvtWeek1$Arrest)
hist(mvtWeek1$Date, breaks = 100)
boxplot(mvtWeek1$Date~mvtWeek1$Arrest)
table(mvtWeek1$Year, mvtWeek1$Arrest)

sort(table(mvtWeek1$LocationDescription))

Top5 <- subset(mvtWeek1, LocationDescription %in% c("STREET", " PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL"))
nrow(Top5)
Top5 <- subset(mvtWeek1, LocationDescription == "STREET" | LocationDescription == "ALLEY" | LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$Arrest, Top5$LocationDescription)
table(mvtWeek1$Weekday, mvtWeek1$LocationDescription)
