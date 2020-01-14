library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

#Year 2007
gapminder_2007 <- filter(gapminder, year == 2007)

#Plot + line for lifeExp
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point(shape = 18) +
  geom_line()

#Plot  line for total Pop
ggplot(gapminder_2007, aes(x = gdpPercap, y = pop, color = continent)) + 
  geom_point(shape = 20) +
  geom_line()

#BARPLOS
#1. LifeExp in Asia over years
gapminder %>%
  filter(continent == 'Asia') %>%
  ggplot(aes(x = year, y = lifeExp, fill = factor(country))) + 
           geom_bar(stat = 'identity')

#2. Population in Oceania from 1992 to 2007
gapminder %>%
  filter(continent == 'Oceania', year >= 1992) %>%
  ggplot(aes(x = country, y = pop/(10^6), fill = factor(year))) + 
    geom_bar(stat = 'identity')

#3. Population in different continents in 2007
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(TotalPopulation = sum(as.numeric(pop))) %>%
  ggplot(aes(x = continent, y = TotalPopulation)) +
  geom_bar(stat = 'identity', fill = 'white', color = 'pink')

#4. Mean Life Expectancy in 1997
gapminder %>%
  filter(year == 1997) %>%
  group_by(continent) %>%
  summarise(MeanLifeExp = mean(as.numeric(lifeExp)), sdLifeExp = sd(as.numeric(lifeExp))) %>%
  ggplot(aes(x = continent,
             y = MeanLifeExp,
             ymin = (MeanLifeExp - sdLifeExp),
             ymax = (MeanLifeExp + sdLifeExp))) + 
  geom_bar(stat = 'identity', fill = 'white', color = 'pink', size = 2) + 
  geom_errorbar(width = 0.3, color = 'red')