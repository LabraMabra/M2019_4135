library(tidyr)


df = readRDS("~/Downloads/weather.rds")
gathered_df = gather(df, day, value, X1:X31, na.rm = T)
united_time_df = unite(gathered_df, date, year, month, day, sep = "-")
united_time_df$date = as.Date(gsub("X", "", united_time_df$date))
out_df = pivot_wider(united_time_df, id_cols = date, names_from = measure, values_from = value)
out_df$PrecipitationIn <- as.numeric(out_df$PrecipitationIn)
out_df$PrecipitationIn[is.na(out_df$PrecipitationIn)] <- median(na.omit(out_df$PrecipitationIn[out_df$PrecipitationIn>0]))
