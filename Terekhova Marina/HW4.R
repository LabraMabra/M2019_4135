library(tidyr)
weather2 <- gather(weather, day, value, X1:X31)
weather2$day <- sapply(weather2$day, function(x) gsub("X", "", x))
weather2 <- unite(weather2, data, day, month, year, sep = ".")
weather2$X <- NULL
weather2 <- spread(weather2, measure, value)
sum(is.na(weather2))
weather2 <- weather2[complete.cases(weather2),]
