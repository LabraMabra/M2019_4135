library(tidyr)
library(dplyr)
#Please, use these libraries
weather <- readRDS('weather.rds')
weather <- weather[,-1]
weather <- gather(weather,day,measures,c(4:34))
weather <- weather %>% separate(day,c('del','day'),sep='X')%>%
  select(-"del")
#I don't like X at the begining
weather <- spread(weather,measure,measures)
weather[,c(1:3)] <- sapply(weather[,c(1:3)],as.numeric)
weather <- unite(weather, Date, day, month, year, sep="/")
weather$Date <- lubridate::dmy(weather$Date)
#Here you can see warnings but it is good, because dates like 30 Feb failed, they don't even exist
weather <- dplyr::arrange(weather,Date)
#Here we can see at the end NAs, we easily can drop it, cause we don't loose the data
weather <- weather[which(rowMeans(is.na(weather))<0.5),]
weather <- mutate(weather,Events=ifelse(Events=="", "No events", Events))
weather[rowSums(is.na(weather))>0,]
#Checking where are NA and supposing what we can do
wNA <- which(is.na(weather$Max.Gust.SpeedMPH))
weather[-c(1,3,22)] <- sapply(weather[-c(1,3,22)], as.numeric)
#Making as.numeric data with numbers
weather$Max.Gust.SpeedMPH[wNA] <- sapply(wNA, function(i) with(weather, mean(c(Max.Gust.SpeedMPH[i-5],Max.Gust.SpeedMPH[i+5]))))
table(weather$PrecipitationIn)
#See that we don't have 1
weather$PrecipitationIn <- as.numeric(weather$PrecipitationIn)
weather$PrecipitationIn[is.na(weather$PrecipitationIn)] <- sapply(which(is.na(weather$PrecipitationIn)), function(i) with(weather, round(mean(c(PrecipitationIn[i-5],PrecipitationIn[i+5]),na.rm = TRUE),digits=2)))                                         
