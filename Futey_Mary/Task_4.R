
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)
library(anchors)


#load data
weather <- readRDS("~/Desktop/weather.rds")

#gather days to rows 
weather_2 <- gather(weather, day, val, X1 : X31, na.rm = TRUE)

#need to remove "X" from day variables
weather_no_x <- weather_2 %>% separate(day,c('del','day'),sep='X')%>%select(-"del")

#unite date to one column
weather_united <- unite(weather_no_x, date, year, month, day, sep="/")

#format date
weather_united$date <- lubridate::dmy(weather_united$date)

#spread measurements to columns
weather_spread <- spread(weather_united, measure, val)

#delete "X" column
weather_spread$X <- NULL

#remove the NA values
weather_spread_omit <- setDT(weather_spread)[, lapply(.SD, na.omit), by = date]

#convert T to Trace
weather_trace <- replace.value(weather_spread_omit, "PrecipitationIn", from="T", to="Trace")

#check structure
str(weather_trace)






