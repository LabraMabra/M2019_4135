
#first, let's get rid of X in days columns
names(weather)[5:35] <- 1:31
#all days in one column
weather_fixed_days <- gather(weather, day, val, 5 : 35, na.rm = TRUE)
#each variable in different column
weather_fixed_variables <- spread(weather_fixed_days, measure, val)
#unite day, month and year in date
weather_with_date <- unite(weather_fixed_variables, y_m_d, year, month, day)

