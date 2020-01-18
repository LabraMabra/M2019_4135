weather <- readRDS("~/Downloads/weather.rds")
#first, let's get rid of X in days columns
names(weather)[5:35] <- 1:31
#all days in one column
weather_fixed_days <- gather(weather, day, val, 5 : 35, na.rm = TRUE)
#unite day, month and year in date
weather_with_date <- unite(weather_fixed_days, y_m_d, year, month, day)

#date format
weather_with_date <- lubridate::ymd(weather_with_date$y_m_d)

#remove X col
weather_with_date$X <- NULL
#var-col, obs-row
weather_spread<- spread(weather_with_date, measure, val)

#replace NA in Events with meaningful values ("Sunny")
weather_spread$Events[weather_spread$Events %in% ""] <- "Sunny"
#replace trace amounts with "0"
weather_spread$PrecipitationIn[weather_spread$PrecipitationIn %in% "T"] <- "0"
