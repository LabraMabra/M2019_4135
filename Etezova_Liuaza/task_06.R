library(tidyr)
library(ggplot2)
library(gapminder)

# Task 06
# A
iris %>%
  unite(col = Sepal, Sepal.Length, Sepal.Width) %>%
  unite(col = Petal, Petal.Length, Petal.Width) %>%
  pivot_longer(c(Sepal, Petal), names_to = "Part", values_to = "Value") %>%
  separate(col = Value, into = c("Length", "Width"), sep = '_') %>%
  mutate_at(vars(c(Length, Width)), as.numeric) ->
  iris_long

ggplot(iris_long, aes(x = Length, y = Width, color = Part)) + 
  geom_point() 

# B
ggplot(filter(gapminder, year == 2007), aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + 
  scale_x_log10()

gapminder %>%
  select(-country) %>%
  group_by(continent, year) %>%
  mutate(meanLifeExp = mean(lifeExp)) ->
  gapminder_b
ggplot(gapminder_b, aes(x = year, y = meanLifeExp, color = continent)) + 
  geom_point() + 
  expand_limits(y = 0)
ggplot(gapminder_b, aes(x = year, y = meanLifeExp, color = continent)) + 
  geom_line() + 
  expand_limits(y = 0)


# Task 07
# 1
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() +
  facet_wrap(~year, nrow = 3, ncol = 4) + 
  scale_x_log10()

# 2
airquality %>%
  pivot_longer(1:4, names_to = "Measure", values_to = "Value") ->
  airquality_2
ggplot(airquality_2, aes(x = Day, y = Value, color = Measure)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(Measure~Month, scales = "free")

# 3
ggplot(airquality, aes(x = Temp, fill = as.factor(Month))) +
  geom_density(alpha = 0.5)