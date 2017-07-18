mvt <- read.csv("C:/Users/Nishank/Desktop/SNU/RStuff/mvt.csv", stringsAsFactors = FALSE)
str(mvt)

plot(mvt$Longitude, mvt$Latitude)

#converting to usable format

mvt$Date <- strptime(mvt$Date, "%m/%d/%y %H:%M")
class(mvt$Date)

#weekday + hour
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)
str(mvt$Date)

table(mvt$Weekday)
WeekdayCounts <- as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

library(ggplot2)
#added chronological ordering
ggplot(data = WeekdayCounts, mapping = aes(x = factor(Var1, ordered = TRUE, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")), y = Freq)) + geom_line(aes(group = 1),alpha = 0.3) + xlab("Day of the week") + ylab("Total motor vehicle thefts")

DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)
DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))

#group divides the number of lines
ggplot(data = DayHourCounts, mapping = aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1, color  = Var1),size = 2)

DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

#Heatmap
ggplot(data = DayHourCounts, mapping = aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts", low = "white",high = "red") + theme(axis.title.y = element_blank())
