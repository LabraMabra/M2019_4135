library(data.table)
library(anchors)
library (tidyr)
library (dplyr)
library (lubridate)

weather <- readRDS("/Users/Lisa/Downloads/weather (1).rds")
weather1 <- dplyr::select(weather, -1)
weather2 <- gather(weather1 , day, val, X1:X31, na.rm = TRUE)
weather2_x_rem <-  weather2 %>% separate(day, c('del', 'day'), sep ='X') %>% select(-"del")
weather_common <- unite(weather2_x_rem, date, year, month, day, sep = "-")
weather_common$date <- as.Date(gsub("X", "", weather_common$date))
final_df = pivot_wider(weather_common, id_cols = date, names_from = measure, values_from = val)

final_df$PrecipitationIn <- final_df$PrecipitationIn %>% replace(final_df$PrecipitationIn == 'T', '-1')
final_df <- final_df %>% mutate(PrecipitationIn = as.numeric(PrecipitationIn))
View(final_df)

