library(tidyr)

#load data
weather_2 <- readRDS("~/Desktop/weather.rds")

#set numeric vector
numeric <- c(weather[1:20,4], weather[22,4])

#gather days to rows 
weather_2 <- gather(weather, day, val, X1 : X31, na.rm = TRUE)

#need to remove "X" from day variables
weather_no_x$day <- gsub("X","",weather_no_x$day)

#unite date to one column
weather_united <-   unite(weather_no_x, date, day, month, year, sep = "/")

#spread columns
weather_spread <- spread(weather_united, measure, val)

#convert to numeric
weather_final[numeric] <- sapply(weather_final[numeric],as.numeric)

#convert T to trace
weather_final[is.na(weather_final)]<-"Trace"

#reorder columns
weather_final_2 <- weather_final[, c(2, 4, 11, 1, 3, 5:10, 12: 23)]

View(weather_final_2)



