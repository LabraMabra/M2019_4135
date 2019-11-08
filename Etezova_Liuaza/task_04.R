library(tidyr)

rds <- readRDS("weather.rds")
rds$X <- NULL
rds <- gather(rds, day, measure_temp, -c(year, month, measure), na.rm = TRUE)
rds$day <- gsub("X", "", rds$day)
rds <- unite(rds, Date, year, month, day, sep = "-", na.rm = TRUE)
rds$Date <- as.Date(rds$Date)
rds <- pivot_wider(rds, names_from = measure, values_from = measure_temp)