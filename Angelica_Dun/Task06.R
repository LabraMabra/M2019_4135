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

#B2
#mean life exp (line+scatter)
gapminder %>%
  select(continent, year, lifeExp) %>%
  group_by(continent) %>%
  group_by(year, add = T) %>%
  summarise(meanLifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = meanLifeExp, color = continent)) +
  geom_point() +
  geom_line()

#total Pop
gapminder %>%
  select(continent, year, pop) %>%
  group_by(continent) %>%
  group_by(year, add = T) %>%
  summarise(totalPop = sum(pop)) %>%
  ggplot(aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  geom_line() +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))

#Barplots
ggplot(gapminder, aes(x=continent, fill=continent)) + geom_bar()
#population per continent at the start and at the end of observation
gapminder %>% filter(year == 2007 | year == 1952) %>% 
  ggplot(aes(as.factor(year), y = pop, fill = continent))  +
  geom_bar(stat = 'identity')
