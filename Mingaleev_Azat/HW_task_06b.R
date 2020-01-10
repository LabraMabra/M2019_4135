library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)

Return_me_2007 <- filter(gapminder, year == 2007)

# Size of points set by population size (pop variable)
ggplot(Return_me_2007, aes(x = gdpPercap, y =lifeExp, 
                           color = continent,size = pop)) +
  geom_point()

total_data <- gapminder %>%  
  group_by(continent, year) %>%
  summarise(meanlifeExp = mean(lifeExp))

# meanlifeExp ~ year

# points
total_data %>%
  ggplot(aes( x = year, y = meanlifeExp, color = continent)) +
  geom_point() 

# lines
total_data %>%
  ggplot(aes( x = year, y = meanlifeExp, color = continent)) +
  geom_line()

# Boxplot. 

gapminder %>% ggplot(aes(x = continent, y = gdpPercap,
                         color = continent)) + geom_boxplot()


# Densities. Africa have separate plot,
# because changes in scale make unreadable plot with all densities (too small)

filter(gapminder, 
       continent == "Africa") %>% ggplot(aes(x = gdpPercap,
                                                        fill = continent,
                                                        color = continent)) + geom_density(alpha = 0.4)

filter(gapminder, continent != "Africa") %>% ggplot(aes(x = gdpPercap,
                        fill = continent,
                        color = continent)) + geom_density(alpha = 0.4)

# Barplot

gapminder %>% select(gdpPercap,
                     continent) %>%
  group_by(continent) %>%
  summarise(mean_gdp = mean(gdpPercap),
            sd_gdp = sd(gdpPercap)) %>% mutate(lower = mean_gdp - 3*sd_gdp,
                                               upper = mean_gdp + 3*sd_gdp) %>%
  ggplot(aes(x = continent,
         y = mean_gdp,
         ymin = lower,
         ymax = upper)) + geom_bar(stat = "identity",
                                   fill = "white",
                                   color = "black") + geom_errorbar(width = 0.2,
                                                                    size = 1)


