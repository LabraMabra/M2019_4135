
library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)

#I combined the two graphing tasks into one for task 6

#First half of task 6 from slides on Nov. 20

#Part A: iris_long plot
iris_task6 <- iris
iris_long <- iris_task6 %>% tibble::rowid_to_column() %>% 
  gather(key = "Part", value = "Measurement" , 2:5) %>% 
  separate(Part, c("Part", "Dimension")) %>% 
  spread(key = Dimension, value = Measurement)

ggplot(iris_long, 
       aes(x = Length,
           y = Width, 
           color = Part)) +
  geom_point(shape = 20, 
             size = 2) +
  labs(color = "Flower Part")


#Part B: gapminder plots
gap_mind <- gapminder


#Year 2007: life exp and GDPpercap
gap_2007 <- gap_mind[gap_mind$year == 2007, ]

ggplot(gap_2007,
       aes(x = gdpPercap, 
           y = lifeExp,
           size = pop,
           color = continent)) +
       geom_point(alpha = 0.7) +
  xlab("GDP per Capita") +
  ylab("Mean Life Expectancy") +
  labs(size = "Population", color = "Continent") +
  scale_x_log10()
  

#Mean life exp vs year scatter plot
ggplot(data = gap_mind %>% 
         group_by(year, continent) %>% 
         summarise(meanLifeExp = mean(lifeExp)),
       aes(x = year, 
           y = meanLifeExp,
           color = continent)) +
  geom_point(shape = 20) +
  xlab("GDP per Capita") +
  ylab("Mean Life Expectancy") +
  ylim(c(0, 85)) +
  labs(color = "Continent")

#Mean life exp vs year line plot
ggplot(data = gap_mind %>% 
         group_by(year, continent) %>% 
         summarise(meanLifeExp = mean(lifeExp)),
       aes(x = year, 
           y = meanLifeExp,
           color = continent)) +
  geom_line(stat = "identity") + 
  xlab("Year") +
  ylab("Mean Life Expectancy") +
  labs(color = "Continent") +
  ylim(c(0, 85)) +
  ggtitle("Mean life expectancy for each continent")


#Gapminder barplots
#US vs UK life expectancy by year
US_UK <- gapminder %>%
  filter(country %in% c("United Kingdom", "United States"))

ggplot(US_UK, aes(x = year, 
                  y = lifeExp,
                  fill = country)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Year") +
  ylab("Life Expectancy") +
  labs(fill = "Country") +
  ggtitle("US vs UK Life Expectancy")

#US vs UK population by year
ggplot(US_UK, aes(x = year, 
                  y = pop,
                  fill = country)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  xlab("Year") +
  ylab("Population") +
  labs(fill = "Country") + 
  ggtitle("US vs UK population by year")

  
################

#Second half of task 6 from slides on Dec. 4

library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)

# Data: Gapminder dataset, All years facet
gap_mind <- gapminder
ggplot(gap_mind,
       aes(x = gdpPercap, 
           y = lifeExp,
           size = pop,
           color = continent)) +
  geom_point(alpha = 0.7) +
  xlab("GDP per Capita") +
  ylab("Mean Life Expectancy") +
  labs(size = "Population", color = "Continent") +
  facet_wrap(. ~ year, nrow = 3, ncol = 4) +
  scale_x_log10()


# Data: Airquality, transform, plot all measures by time
air_qual <- airquality
air_qual_trans <- air_qual %>% 
  gather(Measure, Value, Ozone:Temp)

ggplot(air_qual_trans, 
       aes(x = Day, 
           y = Value,
           color = Measure)) +
  facet_grid(Measure~Month, scales="free_y") + 
  geom_line() +
  geom_point(shape = 21, size = 2, fill = 'white')


# Some numerical data: distributional plots
ggplot(air_qual, 
       aes(x = Wind,
           fill = as.factor(Month))) +
  geom_histogram(position = "dodge", bins = 10) +
  ylab("Count") +
  labs(fill = "Month")

ggplot(air_qual, 
       aes(x = Temp,
           fill = as.factor(Month))) +
  geom_histogram(position = "dodge", bins = 10) +
  ylab("Count") +
  labs(fill = "Month")






