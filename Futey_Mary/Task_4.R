
library(tidyr)

#load data
weather<- readRDS("~/Desktop/weather.rds")

#set numeric vector
num <- c(weather[1:20,4], weather[22,4])

#gather days to rows 
weather_2 <- gather(weather, day, variable, X1:X31, na.rm = T)

#need to remove "X" from day variables
weather_2$day <- gsub("X","",weather_2$day)

#unite date to one column
weather_d <-  unite(weather_2, date, day, month, year, sep = "/")

#format date
weather_d$date <- as.Date(weather_d$date,"%d/%m/%Y")

#spread columns
weather_final <-  pivot_wider(weather_d, id_cols = date, names_from = measure, values_from = variable)

#convert T to trace
weather_final$PrecipitationIn <- gsub("T","0.00",weather_final$PrecipitationIn)

#convert to numeric
weather_final[num] <- sapply(weather_final[num],as.numeric)
View(weather_final)



