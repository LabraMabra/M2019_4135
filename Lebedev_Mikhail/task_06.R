library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)
library(cowplot)

# A

iris_long <- iris %>% mutate(row_num = row_number()) %>%
  pivot_longer(cols = Sepal.Length:Petal.Width, names_to = "measurement") %>%
  separate(measurement, into = c("Part", "Measure")) %>%
  pivot_wider(names_from = "Measure", values_from = "value") %>%
  select(-row_num)


kek <- gapminder %>%
  filter(year == 2007)

# B1:
ggplot(kek, mapping = aes(x =  gdpPercap, y = lifeExp, color = continent, size = pop)) + geom_point() + scale_x_log10() 
  #geom_boxplot() +
  #geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

# B2:
a <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)), mapping = aes(x = year, y = meanLifeExp,  color = continent)) + geom_line() + ylim(0, 80)
b <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)), mapping = aes(x = year, y = meanLifeExp,  color = continent)) +  geom_jitter() + ylim(0, 80)
plot_grid(a, b)

# B3:
ggplot(gapminder %>% filter(year == 2007) %>% filter(country %in% c("Afghanistan", "Saudi Arabia", "Iran", "Pakistan")), mapping = aes(country, lifeExp)) + geom_bar(stat = "identity")
ggplot(gapminder %>% filter(year == 1992) %>% filter(continent == "Europe"), mapping = aes(country, gdpPercap)) + geom_bar(stat = "identity")
ggplot(gapminder %>% filter(year == 2007) %>% filter(continent == "Europe"), mapping = aes(country, gdpPercap)) + geom_bar(stat = "identity")



#View(kek)