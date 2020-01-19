library(ggplot2)
library(tidyr)
library(tidyverse)

#A
#long iris 
iris_long <- iris  %>% 
  mutate(id = 1:nrow(iris)) %>% #without id 'spread' function wasn't working properly
  gather(flower_part, value, 1:4 ) %>%  #we have 3 columns now: species, flower part(SL, SW, PL, PW) and value 
  separate(flower_part, c('flower_part', 'dim'))  %>%    #we separate type of measure (l/w) from the type of flower part 
  #now we can divide length and width
  spread(dim, value)
  #scatter plot
ggplot(iris_long, aes(x=Width, y=Length, col = flower_part))+
  geom_point(size = .5)

#B
#2007
filter(gapminder, year == 2007) %>%
  ggplot(aes(x= gdpPercap,
             y = lifeExp,
             size = pop,
             color = continent))+
  geom_point()  +labs(size = "Population", color = "Continent") + scale_x_log10()
