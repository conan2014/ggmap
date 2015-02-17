library(dplyr)
install.packages("date")
library(date)
a = read.csv("C:/Users/Conan/Desktop/AK.csv")
col <- c("Year", "City", "State", "Shape", "Duration", "Summary", "Posted")
names(a) <- col
fix(a)


a <- mutate(a, "year" = 0)
for (i in 1:422){
  datetxt<-as.Date(a$Year[i],"%m/%d/%Y")
  a$year[i] <- as.numeric(format(datetxt, format = "%Y"))
  
}
class(a$year[1])
df <- filter(a, year >= 2000) 
fix(df)
