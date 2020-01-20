library(tidyr)
library(dplyr)
library(ggplot2)
library(gapminder)

#Task 06 A: Iris to Iris long and plot
iris_long1 <- reshape(iris, varying=list(c("Sepal.Length","Petal.Length"),c("Sepal.Width", "Petal.Width")), v.names=c("Length", "Width"), timevar="Part", times=c("Sepal", "Petal"), direction="long")

iris_long2 <- iris_long1[,1:4]

ggplot(iris_long2,aes(x = Length, y = Width, color =Part)) +geom_point() 

#Task 06 B1: 2007
filter(gapminder, year == 2007) %>% ggplot(aes(x= gdpPercap,y = lifeExp,size = pop,color = continent))+
  geom_point()  +labs(size = "Population", color = "Continent") + scale_x_log10()
  
#Task 06 B2: scatter + line
grouped_gapminder <- group_by(gapminder,year,continent)
summarised_gapminder <- summarise(grouped_gapminder, mean_life_exp = mean(lifeExp)) 
  
ggplot(summarised_gapminder, aes(x = year, y = mean_life_exp, color = continent)) + geom_point() + ylim(0,80)
ggplot(summarised_gapminder, aes(x = year, y = mean_life_exp, color = continent)) + geom_line() + ylim(0,80)

#Task 06 B3: custom

iris %>% 
  select(Sepal.Length, Species) %>% 
  group_by(Species) %>% 
  summarise(meanSL = mean(Sepal.Length), sdSL = sd(Sepal.Length)) %>% 
  mutate(lower = meanSL - 2* sdSL, upper = meanSL + 2* sdSL) %>% 
  ggplot(aes(x = Species, y = meanSL, ymin = lower, ymax = upper)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_errorbar(width = 0.2, size = 2)

CO2 %>% 
  select(Plant, uptake) %>% 
  group_by(Plant) %>% 
  summarise(meanSL = mean(uptake), sdSL = sd(uptake)) %>% 
  mutate(lower = meanSL - 2* sdSL, upper = meanSL + 2* sdSL) %>% 
  ggplot(aes(x = Plant, y = meanSL, ymin = lower, ymax = upper)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_errorbar(width = 0.2, size = 2)
  



