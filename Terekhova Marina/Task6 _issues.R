library(dplyr)
library(tidyr)
library(ggplot2)

#Part A. Transforming iris into iris_long and creating a plot

iris_long <- iris %>% 
  mutate(row = row_number()) %>% 
  gather(key = "attributes", value = "values", Sepal.Length:Petal.Width) %>% 
  separate(attributes, into = c("Part", "mesuare"))  %>% 
  spread(mesuare, values) %>%  
  select(-row)

#scatterplot
ggplot(iris_long, aes(x = Length, y = Width, color = Species)) +
  geom_point(shape = 21, size = 3, fill = 'white')

#bar plot
iris_long %>% select(Length, Species) %>%
  group_by(Species) %>%
  summarise(meanSL = mean(Length),
            sdSL = sd(Length)) %>% 
  mutate(lower = meanSL - 2*sdSL,
         upper = meanSL + 2*sdSL) %>%
  ggplot(aes(x = Species,
             y = meanSL,
             ymin = lower,
             ymax = upper)) +
  geom_bar(stat = 'identity', fill = 'white', color = 'black') +
  geom_errorbar(width = 0.1, size = 1)

#Part B. Gapminder dataset
#In the task it was unclear about years, because if I make a scatter for 2007 - it will be only one point on the plot for each continent 
#and populaton over the years - it will be a messy
#So, I made it in the way like it was on lecture slides after task description (scatter over the years and pop for 2007)

install.packages("gapminder")
library(gapminder)

#Scatter + line at one plot for mean LifeExp over years
gapminder %>%
  group_by(continent, year) %>%
  summarise(meanlifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = meanlifeExp, color = continent)) +
  geom_line(size = 1) + 
  geom_point() +
  ylim(0, 85)


#Population for 2007 year
gapminder %>%
  filter(year == 2007) %>% 
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = continent)) +
  geom_point()  +
  scale_x_log10()

#Barplot_1 - Mean life expectancy at birth in 2007 over the continents
gapminder %>% filter(year == 2007) %>% 
  select(lifeExp, continent) %>%
  group_by(continent) %>%
  summarise(meanExp = mean(lifeExp),
            sdExp = sd(lifeExp)) %>% 
  mutate(lower = meanExp - 2*sdExp,
         upper = meanExp + 2*sdExp) %>%
  ggplot(aes(x = continent,
             y = meanExp,
             ymin = lower,
             ymax = upper)) +
  geom_bar(stat = 'identity', fill = 'white', color = 'black') +
  geom_errorbar(width = 0.1, size = 1)
#Barplot_2 - Difference between 1952 and 2007 years in  life expectancy over the continents

gapminder %>% filter(year == 2007 | year == 1952) %>% 
  ggplot(aes(as.factor(year), y = lifeExp, fill = continent))  +
  geom_bar(stat = 'identity')

#Visualizing data (part 2)
#Gapminder dataset all years facet

gapminder %>%
  ggplot(aes(gdpPercap, lifeExp, size = pop, colour = continent)) +
  geom_point() +
  scale_size(range = c(1, 6)) +
  facet_wrap(~ year) +
  scale_x_log10()

#Airquality plotting measures by time

airq <- gather(airquality, Measure, Value, Ozone:Temp )
airq %>%
  ggplot(aes(Day, Value, size = pop, color = Measure)) +
  geom_line(size = 1) + 
  geom_point(shape = 21, size = 2, fill = 'white') +
  facet_grid(Measure ~ Month, scales="free_y") 

# Distributional plots for some numerical data
#1 Wind value distribution  
plot1 <- airquality %>%  ggplot(aes(x = Wind, fill = as.factor(Month)))
plot1 + geom_histogram(position = 'dodge', bins = 10)

#2 Density for temperature 
plot2 <- airquality %>%  ggplot(aes(x = Temp, fill = as.factor(Month)))
plot2 + geom_histogram(position = 'identity', bins = 10, aes(y = ..density..), alpha = 0.6) +
  geom_density(alpha = 0)
