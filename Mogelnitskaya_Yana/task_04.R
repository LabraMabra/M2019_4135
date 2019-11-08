library(tidyr)
library(dplyr)
library(lubridate)
#Part 1. Loading data
rds <- readRDS("~/Desktop/RStudio/weather.rds")

#Part 2. Reshaping data : column X was removed,day columns were renamed (X1 -> 1), days were gathered to rows, 
#date was united and its type was changed, then measure column was spreaded
rds <- rds %>% select(-1) 
names(rds) <- c(names(rds)[1:3], 1:31) 
rds <- gather(rds, day, values, 4:34, na.rm=TRUE) 
rds <- unite(rds, date, year, month, day, sep="-")
rds$date <- lubridate::ymd(rds$date)
rds<- spread(rds, measure, values)

#Part 3. Removing missing values : changed to "No events" in Event column and mean value in numeric columns
rds <- mutate(rds, Events=replace(rds$Events, which(rds$Events=="") ,"No events"))
rds[, -c(1,3,22)] <- sapply(rds[, -c(1,3,22)], as.numeric)
for (i in c(2,4:21,23)){
  rds[,i] <- replace(rds[,i], which(is.na(rds[,i])), mean(rds[,i], na.rm=TRUE))}


