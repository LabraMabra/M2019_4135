library(tidyr)
weather <- readRDS('weather.rds')
weather <- gather(weather, day, value, X1:X31)
weather$day <- sapply(weather$day, function(x) gsub("X", "", x))
weather <- unite(weather, data, day, month, year, sep = "-")
weather$X <- NULL
weather <- spread(weather, measure, value)
weather$data <- as.Date(weather$data)
sum(is.na(weather))
weather <- weather[complete.cases(weather),]
#(for PrecipitationIn, there are “T” values indicating “Trace”, or 0 in number)
weather$PrecipitationIn[weather$PrecipitationIn %in% "T"] <- "0"
weather[, !names(weather) %in% c("data", "Events")] <- sapply(weather[, !names(weather) %in% c("data", "Events")], as.numeric)
