library(ggplot2)
library(gapminder)

# Part 1
# A
part <- c()
length <- c()
width <- c()
species <- c()
for(i in 1:nrow(iris)) {
  index <- i * 2 - 1
  species[index] <- iris$Species[i]
  part[index] <- 'Petal'
  length[index] <- iris$Petal.Length[i]
  width[index] <- iris$Petal.Width[i]
  
  index <- i * 2
  species[index] <- iris$Species[i]
  part[index] <- 'Sepal'
  length[index] <- iris$Sepal.Length[i]
  width[index] <- iris$Sepal.Width[i]
}

iris_long <- data.frame('Species' = factor(species, labels=c('setosa', 'versicolor', 'virginica')),
                        'Part' = part, 'Length' = length, 'Width' = width)

ggplot(iris_long, aes(x = Length, y = Width, color = Part)) + 
  geom_point() 


# B
ggplot(gapminder[gapminder$year == 2007, ], aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point() + 
  geom_line()

ggplot(gapminder, aes(x = year, y = pop, color = continent)) + 
  geom_point()


# Part 2
# 1
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point() +
  facet_wrap(~ year, nrow = 3, ncol = 4)