
library(tidyr)
library(lubridate)
weather <- readRDS("~/Downloads/weather.rds")
#first, let's get rid of X in days columns
names(weather)[5:35] <- 1:31
#############UPD##########
measures <- c(weather[1:20,4], weather[22,4])

#all days in one column
weather_fixed_days <- gather(weather, day, val, 5 : 35, na.rm = TRUE)
#unite day, month and year in date
weather_with_date <- unite(weather_fixed_days, y_m_d, year, month, day)

#date format
########There is probably a mistake in syntax or smth else because I got unknown format for date column 
weather_with_date$y_m_d <- ymd(weather_with_date$y_m_d)

#remove X col
weather_with_date$X <- NULL
#var-col, obs-row
weather_spread<- spread(weather_with_date, measure, val)

############UPD################convert all to num
weather_spread[measures] <- sapply(weather_spread[measures],as.numeric)
str(weather_spread)


#replace NA in Events with meaningful values ("Sunny")
weather_spread$Events[weather_spread$Events %in% ""] <- "Sunny"
#replace trace amounts with "0"
weather_spread$PrecipitationIn[weather_spread$PrecipitationIn %in% "T"] <- "0"
