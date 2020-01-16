library(ggplot2)
library(tidyr)
library(dplyr)
library(gapminder)
data("iris")
data("gapminder")
data("airquality")

# 1
ggplot(gapminder, aes(gdpPercap, lifeExp, color=continent, size=pop))+
  geom_point()+
  scale_x_log10()+
  facet_wrap(~year, nrow=3, ncol=4, scales = 'fixed')+
  scale_size_continuous(range = c(.4, 3))+
  theme(aspect.ratio = 1)+
  theme(axis.text=element_text(size=7))

# 2
View(airquality)
airquality <- gather(airquality, Measure, Value, 1:4) 

ggplot(airquality, aes(x=Day, y=Value, col = Measure))+
  facet_grid(Measure~Month, scales = 'free')+
  geom_line()+
  geom_point(size=.8)+
  theme(aspect.ratio = 1)

# 3 boxplots of iris petal&sepal length
View(iris)
iris %>% 
  mutate(id = 1:nrow(iris)) %>% 
  gather(key = trait, value = measurement, 1:4) %>% 
  separate(trait, c('Part', 'measure'))  %>% 
  spread(measure, measurement) -> 
  iris_long

ggplot(iris_long, aes(Species, Length, fill=Species))+
  geom_boxplot()+
  facet_grid(.~Part)+
  geom_jitter(aes(color=Part), width=.1, alpha=.5)+
  theme_bw()+
  theme(aspect.ratio = 1.5)