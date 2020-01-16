library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)

#Gadminder: All years facet
gapminder %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  facet_wrap(~year) +
  scale_x_log10() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

#Airquality: transform, all measures by time
airquality %>%
  gather("Measures", "Value", 1:4) %>%
  group_by(Measures) %>%
  group_by(Month, add = T) %>%
  ggplot(aes(x = Day, y = Value, color = Measures)) +
  facet_grid(Measures ~ Month, scales = 'free_y') +
  geom_point() +
  geom_line()

#Distributional plots for numerical data
