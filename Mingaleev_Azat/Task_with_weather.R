# wheather dataset cleaning
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
library("tidyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")

weather <- readRDS("weather.rds")[,-1]
parameters <- names(weather)
# delete dummy row-index
weather <- weather %>% 
  unite(date,year,month,sep = "-") %>% 
  gather(day,value,paste0("X",1:31), na.rm = FALSE) %>%
  spread(measure,value)
weather$day <- sub("X","",weather$day)
weather$Events[weather$Events == ""] <- "Sunny"
weather <- unite(weather,date,date,day,sep = "-")
# Add to date column days
all_na <- apply(weather[-(1:3)], MARGIN = 1, FUN = function(x) all(is.na(x)))
# Not all months have 31 days. In this table "dummy" days have only NA's
weather <- weather[!all_na,]
# replace empty event on "Sunny"
weather[c(-(1:3),-22)] <- apply(weather[c(-(1:3),-22)], MARGIN = 2, FUN = as.numeric)
# return numeric type for variables



