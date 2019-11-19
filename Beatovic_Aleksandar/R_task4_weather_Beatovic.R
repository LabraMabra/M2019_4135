#use packages 
library(tidyr)

#import dataset
weather_df_raw <- readRDS("/home/aleksandar/Desktop/R course/weather.rds")

#extract measurement columns that will be numeric as vector will need it later...
measurements <- c(weather_df_raw[1:20,4], weather_df_raw[22,4])


#turn X# from wide to long format, na.rm = T if TRUE, will remove rows from output where the value column is NA.
weather_df_X_long <- gather(weather_df_raw, day, variable, X1:X31, na.rm = T)

#delete X from X# gsub replaces first argument for second, in third argument 
weather_df_X_long$day <- gsub("X","",weather_df_X_long$day)



#unite(1 df, 2 name of new column, 3:5 columns to unite, 6 seperator)
weather_df_yyyy_mm_dd = unite(weather_df_X_long, date, day, month, year, sep = "/")


#change date type to date using as date
weather_df_yyyy_mm_dd$date <- as.Date(weather_df_yyyy_mm_dd$date,"%d/%m/%Y")

weather_df_final = pivot_wider(weather_df_yyyy_mm_dd, id_cols = date, names_from = measure, values_from = variable)


#convert to numeric, events cannot be numeric, also in percipitation T means trace parcipitation which means numeric is equal to 0, https://www.thoughtco.com/what-is-trace-of-precipitation-3444238
weather_df_final$PrecipitationIn <- gsub("T","0.00",weather_df_final$PrecipitationIn)

#convert to numeric
weather_df_final[measurements] <- sapply(weather_df_final[measurements],as.numeric)
View(weather_df_final)


