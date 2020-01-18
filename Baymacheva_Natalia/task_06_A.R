library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

new_iris <- cbind(1:150, iris) %>%
  pivot_longer(cols = c(2:5)) %>%
  separate("name", c("Flower_part", "LengthWidth"))

new_iris$`Flower_part` <- as.factor(new_iris$`Flower_part`)
new_iris$`LengthWidth` <- as.factor(new_iris$`LengthWidth`)

iris_long <- pivot_wider(new_iris, names_from = 'LengthWidth', values_from = 'value') %>%
  select(2:5)

View(iris_long)

iris_long %>%  
  ggplot(aes(x = Length, y = Width, color = Flower_part)) +
  geom_point(shape = 6)
