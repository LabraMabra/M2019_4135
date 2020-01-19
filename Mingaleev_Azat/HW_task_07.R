library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
gapminder %>% ggplot(aes(y = lifeExp, x = gdpPercap, 
                                         color = continent,
                         size =  pop)) +
  geom_point(alpha = 0.6) + facet_wrap(~ year,
                                       nrow = 3,
                                       scales = "fixed",
                                       as.table = F) +
  scale_x_log10(breaks = c(1e3,1e4,1e5)) +
  scale_y_continuous(breaks = c(40,60,80))


# Airquality data
air_long <- gather(airquality, parameter, value, c(-Month,-Day))

air_long %>% ggplot(aes(x = Day,
                        y = value,
                        size = I(0.2),
                        color = parameter)) +
  geom_point() +
  geom_line() + 
  facet_grid(parameter  ~ Month, scales = "free")

# Numerical Data from gapminder
gapminder %>% ggplot(aes(x = gdpPercap,
                         fill = continent,
                         color = continent)) + 
  geom_density(alpha = 0.4) +
  facet_grid(continent ~ .)

gapminder %>% ggplot(aes(x = gdpPercap,
                         fill = continent,
                         color = continent)) + 
  geom_histogram(alpha = 0.4) +
  facet_grid(continent ~ .)

# iris

gather(iris, Parameter, value, -Species) %>%
  separate(Parameter, c("Part","Parameter")) %>% ggplot(aes(x = value,
                                                            color = Species,
                                                            fill= Species)) + 
  geom_density(alpha = 0.2) + facet_grid(Parameter ~ Part)








