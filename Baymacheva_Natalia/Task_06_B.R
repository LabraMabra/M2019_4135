library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

#Year 2007
gapminder_2007 <- filter(gapminder, year == 2007)

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() +
  scale_x_log10()

#Plot + line for meanLifeExp
gapminder %>%
  select(continent, year, lifeExp) %>%
  group_by(continent) %>%
  group_by(year, add = T) %>%
  summarise(meanLifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = meanLifeExp, color = continent)) +
  geom_point() +
  geom_line() +
  expand_limits(y = 0)

#Plot + line for totalPop
gapminder %>%
  select(continent, year, pop) %>%
  group_by(continent) %>%
  group_by(year, add = T) %>%
  summarise(totalPop = sum(as.numeric(pop))) %>%
  ggplot(aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  geom_line() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))

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
