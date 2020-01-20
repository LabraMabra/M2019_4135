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

# B2:
a <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)), mapping = aes(x = year, y = meanLifeExp,  color = continent)) + geom_line() + ylim(0, 80)
b <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)), mapping = aes(x = year, y = meanLifeExp,  color = continent)) +  geom_jitter() + ylim(0, 80)
plot_grid(a, b)

# B3:
ggplot(gapminder %>% filter(year == 2007) %>% filter(country %in% c("Afghanistan", "Saudi Arabia", "Iran", "Pakistan")), mapping = aes(country, lifeExp)) + geom_bar(stat = "identity")
ggplot(gapminder %>% filter(year == 1992) %>% filter(continent == "Europe"), mapping = aes(country, gdpPercap)) + geom_bar(stat = "identity")
ggplot(gapminder %>% filter(year == 2007) %>% filter(continent == "Europe"), mapping = aes(country, gdpPercap)) + geom_bar(stat = "identity")


# 7.1

ggplot(gapminder, mapping = aes(x =  gdpPercap, y = lifeExp, color = continent, size = pop)) + geom_point() + scale_x_log10() + facet_wrap(~ year)


# 7.2

ggplot(airquality %>% pivot_longer(cols = Ozone:Temp, names_to = "measurement"), mapping = aes(x =  Day, y = value, color = measurement)) +
 geom_point() + geom_line() + facet_grid(measurement ~ Month, scales = "free_y")


# 7.3
ggplot(infert, mapping = aes(x = education, y = parity)) + geom_boxplot() + geom_jitter(width = .1, alpha = .5)

ggplot(infert, mapping = aes(x = induced, y = spontaneous, color = education, size = parity)) + geom_jitter(width = .2, height = .2, alpha = .5)






