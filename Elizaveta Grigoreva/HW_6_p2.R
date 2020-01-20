library(ggplot2)
library(gapminder)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(tidyr)

library(boot)


#Gapmineder dataser 07_1
ggplot( gapminder,(aes(x = gdpPercap, y =lifeExp, color = continent,size = pop))) +geom_point() +
  facet_wrap(~ year, nrow = 3, ncol = 4) + scale_x_log10() 
#Transform Airquality
air_new <-  tidyr::gather(airquality,Measure, Value, Ozone, Solar.R, Temp, Wind)
ggplot(air_new,(aes(x= Day, y= Value, color= Measure))) + geom_line() + geom_point() +facet_grid( Measure ~ Month, scales = "free_y") 


#Some numerical data
#Frequency death depends of poison
ggplot(poisons,aes(x=poisons$time, fill=poisons$poison)) + xlab("Time") + geom_density(alpha = 0.6 )
#gapminer
ggplot(subset(gapminder),aes(x = year, y = lifeExp, group = country, color = country)) + geom_line( show.legend = FALSE) + facet_wrap(~ continent) +
  scale_color_manual(values = country_colors) + theme_bw() + theme()
#airquality
boxplot(airquality[,0:3], main='Multiple Box plots')
