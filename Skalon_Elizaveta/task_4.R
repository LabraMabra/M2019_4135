library(tidyr)

# read data
df <- readRDS('weather.rds')
# open df in a new tab
View(df)
# check column structure
str(df)
# make df pretty
df_long <- gather(df, day, variable, X1:X31, na.rm = T)
df_united <- unite(df_long, day, day, month, year, sep = '.')
df_united <- df_united[, 2:4]
df_spread <- spread(df_united, measure, variable)
# check data structure
str(df_spread)
# remove X from day name
df_spread$day <- sub('X', '', df_spread$day)
# change data format
df_spread$day <- as.Date(df_spread$day,"%d.%m.%Y")
# substitute T in Precipitation by 0 because T means trace and is equal to 0
df_spread$PrecipitationIn <- sub('T', '0.00', df_spread$PrecipitationIn)
# change characters to numeric
df_spread[,4:23] <- sapply(df_spread[,4:23],as.numeric, na.rm=T)
df_spread$CloudCover <- as.numeric(df_spread$CloudCover)
# add new value instead of blank line
df_spread$Events[df_spread$Events==''] <- 'No_events'
# check NAs
sum(is.na(df_spread))
summary(df_spread)
# replace NAs with mean value of the column
df_spread$Max.Gust.SpeedMPH[is.na(df_spread$Max.Gust.SpeedMPH)] <- 
  mean(df_spread$Max.Gust.SpeedMPH, na.rm = TRUE)
# view new df
View(df_spread)
