library(dplyr)
library(tidyr)
library(ggplot2)
library(gapminder)
#Task 6A
iris <- iris
iris <- iris %>%gather("key", "value", 1:4) %>%separate(key, c("Part", "dim"))
iris <- iris %>%group_by(Part, dim) %>%
  mutate(obs = row_number())
iris_long <- spread(iris, dim, value)
iris_long <- iris_long[,-3]
str(iris_long)
head(iris_long)
ggplot(iris_long, aes(x = Length, y = Width, color = Part))+geom_point()

#Task 6B
gap <- gapminder
gap <- gap%>%select(-country)%>%
  group_by(continent, year)%>%
  mutate(meanLifeExp = mean(lifeExp))%>%
  mutate(pop2 = sum(as.numeric(pop)))%>%
  mutate(gdpPercap2 = sum(as.numeric(gdpPercap)))%>%
  mutate(sdLifeExp = sd(lifeExp))%>%
  mutate(lower = meanLifeExp - 2*sdLifeExp, upper = meanLifeExp + 2*sdLifeExp)
ggplot(gap, aes(x = year, y = meanLifeExp, color = continent))+geom_point()+expand_limits(y = 0)+geom_line()
gap2 <- gap%>%filter(year==2007)
ggplot(gap2, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop))+geom_point()+scale_x_log10()

#Task 6B my own graphs
#Not a barplot, but I really wanted to see this one
ggplot(gap, aes(x = gdpPercap2, y = meanLifeExp, color = continent, size = pop))+geom_point()
gap2 <- ungroup(gap2)
gap2 <- gap2%>%select(continent, meanLifeExp, lower, upper)
gap2 <- unique(gap2)
ggplot(gap2, aes(x = continent, y = meanLifeExp, ymin = lower, ymax = upper))+
  geom_bar(stat = "identity", fill = "white", color = "black")+
  geom_errorbar(width = 0.2, size = 2)

