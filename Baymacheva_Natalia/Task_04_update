library(tidyr)

weather <- readRDS("weather.rds")

# Days are represented as X1-X31, change them to numeric
names(weather)[5:35] <- 1:31

# All the variables are stored in rows, to change it:
gathered <- gather(weather, 'day', 'val', 5:35, na.rm = T)

# To combine colomns with year, month and day:
gathered <- unite(gathered, Day, year, month, day, sep = '-')
gathered$Day <- as.Date(gathered$Day)

# To spread the dataset by each characterictic:
weath <- pivot_wider(gathered, id_cols = Day, names_from = measure, values_from = val)


#Change all numeris frames to numeric data type
#Trace in precipitation means that the amount of precipitation is so small, that cannot be measured
#Change T in precipitation to 0.00
for (col in c(2:21, 23)) {
  weath[col] <- as.numeric(unlist(weath[col]))
}

#Trace in precipitation means that the amount of precipitation is so small, that cannot be measured
#Change T in precipitation to 0.00
for (z in c(1:366)) {
  if (is.na(weath$PrecipitationIn[z])) {
    weath$PrecipitationIn[z] <- 0.00
  }
}

#Change NA in Max.Gust.Speed
for (z in c(1:366)) {
  if (is.na(weath$Max.Gust.SpeedMPH[z])) {
    weath$Max.Gust.SpeedMPH[z] <- 0.00
  }
}


#Change NA in Events to "Sunny and cloudless!"
for (d in c(1:366)) {
  if (weath$Events[d] == '') {
    weath$Events[d] <- 'No precipitation'
  }
}

View(weath)
