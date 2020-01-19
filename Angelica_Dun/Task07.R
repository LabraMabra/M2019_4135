library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

#facet
gapminder %>%
  ggplot(aes(y = lifeExp, x = gdpPercap, 
             color = continent,
             size = pop)) +
  geom_point() +
  facet_wrap(~year) +
  scale_x_log10(breaks = c(1e3,1e4,1e5))+
  scale_y_continuous(breaks = c(40,60,80))


#airquality by time
View(airquality)

airquality %>%
  gather("Measure", "Value", 1:4) %>%
  group_by(Measure) %>%
  group_by(Month, add = T) %>%
  ggplot(aes(x = Day,
             y = Value, 
             color = Measure)) +
  facet_grid(Measure ~ Month, scales = "free_y") +
  geom_point() +
  geom_line()