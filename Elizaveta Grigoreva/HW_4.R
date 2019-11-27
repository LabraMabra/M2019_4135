
library(data.table)
library(anchors)
library (tidyr)
library (dplyr)
library (lubridate)

weather <- readRDS("/Users/Lisa/Downloads/weather (2).rds")
weather1 <- dplyr::select(weather, -1)
weather2 <- gather(weather1 , day, parameter, X1:X31, na.rm = TRUE)

weather2$day <-  gsub(pattern = "X", replacement = "", x = weather2$day, ignore.case = TRUE)
weather2  <- mutate(weather2 , parameter=ifelse(parameter=='', 'no_info', parameter))
weather2 <- unite(weather2 , date, day, month, year, sep='-')


weather2$date <-dmy(weather2$date)
weather2 <- spread(weather2 , measure, parameter)
weather2 [,-c(1,3,22)] <- sapply(weather2 [, -c(1,3,22)], as.numeric)
weather_corrected <- weather2 [c(4:21,23)] <- sapply(c(4:21,23), function(x) replace(data2[,x], which(is.na(data2[,x])), mean(data2[,x], na.rm =T)))
str(weather_corrected)

