library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)

#Part A. Transforming iris into iris_long and creating a plot

iris_long <- iris %>% mutate(row_num = row_number()) %>%
  pivot_longer(cols = Sepal.Length:Petal.Width, names_to = "measurement") %>%
  separate(measurement, into = c("Part", "Measure")) %>%
  pivot_wider(names_from = "Measure", values_from = "value") %>%
  select(-row_num)


#View(iris_long)

#ggplot(iris, aes(x = Sepal.Length, y = Sepal.Widt geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)h, color  = Species)) + geom_point()
#ggplot(iris_long, aes(x = Length, y = Width, color  = Species)) + geom_point()



kek <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent)

# B1:
ggplot(kek, mapping = aes(x = continent, y = lifeExp)) + #geom_point() + 
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = 1/4)

# B2:
ggplot(gapminder, mapping = aes(x = year, y = lifeExp, group = country, color = continent)) + geom_line() + geom_jitter()
ggplot(gapminder, mapping = aes(x = year, y = pop, group = country, color = continent)) + geom_line() + scale_y_log10()


# B3:
ggplot(gapminder %>% filter(year == 2007) %>% filter(country %in% c("Afghanistan", "Saudi Arabia", "Iran", "Pakistan")), mapping = aes(country, lifeExp)) + geom_bar(stat = "identity")



#View(kek)