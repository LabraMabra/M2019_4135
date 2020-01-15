library(ggplot2)
library(tidyr)
library(dplyr)
library(gapminder)
data("iris")
data("gapminder")

### part A
View(iris)
# add ids
iris$id <- 1:nrow(iris)
# make long df
iris %>% 
  gather(key = trait, value = measurement, 1:4) %>% 
  separate(trait, c('Part', 'measure'))  %>% 
  spread(measure, measurement) -> 
  iris_long

# plot
ggplot(iris_long, aes(Length, Width, color=Part))+
  geom_point()

### part B
View(gapminder)
str(gapminder)

## 1
df_2007 <- split(gapminder, gapminder$year==2007)$'TRUE'
ggplot(df_2007, aes(gdpPercap, lifeExp, color=continent, size=pop))+
  geom_point()+
  scale_x_log10()

## 2
# mean_lifeExp
gapminder %>%
  group_by(continent, year) %>%
  summarise(mean_lifeExp=mean(lifeExp)) -> df_mean
ggplot(df_mean, aes(year, mean_lifeExp, color=continent)) +
  geom_point(size=2.5) +
  geom_line(size=.5)

# total_pop
gapminder %>%
  group_by(continent, year) %>%
  summarise(total_pop = sum(as.numeric(pop))) -> df_pop
ggplot(df_pop, aes(year, total_pop, color=continent)) +
  geom_point(size=2.5) +
  geom_line(size=.5) 

## 3
# barplot - min and max life exp
gapminder %>% 
  group_by(continent) %>% 
  summarize(min_life = min(lifeExp),
            max_life = max(lifeExp)) %>%
  gather(measure, life_exp, 2:3) %>% 
  ggplot(aes(continent, life_exp)) +
  geom_bar(stat = "identity", position = position_dodge(.9), aes(fill = measure))
# barplot - check number of measures for each year
ggplot(gapminder, aes(x=year, fill=continent)) + geom_bar()
# boxplot - lifeExp in different continents
ggplot(gapminder, aes(continent, lifeExp, fill=continent)) +
  geom_boxplot(outlier.colour = 'red')+
  theme_bw()
