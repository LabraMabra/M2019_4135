library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
gapminder %>% ggplot(aes(y = log(lifeExp), x = log(gdpPercap), 
                                         color = continent,
                         size =  pop)) +
  geom_point(alpha = 0.6) + facet_wrap(~ year,
                                       nrow = 3,
                                       scales = "free",
                                       as.table = F)

