library(data.table)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggpubr)



#Task A
#Transform data uding reshape function
iris_long_before_transf <- reshape(iris, varying=list(c(1,3),c(2,4)),
                                   v.names=c("Length", "Width"), 
                                   timevar="Part", times=c("Sepal", "Petal"),
                                   idvar="Measure ID", direction="long")

#Delete Measure ID column
iris_long <-iris_long_before_transf [names(iris_long_before_transf) != "Measure ID"]
str(iris_long)
iris_long
#Create_plot
ggplot(iris_long, aes(x=Length, y=Width, color=Part)) + geom_point()

#Task B1
library(gapminder)
#Year 2007 and plot from slide

gapminder_2007 <- filter(gapminder, year == 2007)

ggplot(gapminder_2007,aes(x= gdpPercap,y = lifeExp,size = pop,color = continent))+
  geom_point()  +labs(size = "Population", color = "Continent") + scale_x_log10()

#Task B2
#Scatter+Line for lifeEXP and combine it
A <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)),aes(x = year, y = meanLifeExp,color = continent)) + ylim(0,80) +geom_point()
B <- ggplot(gapminder %>% group_by(year, continent) %>% summarise(meanLifeExp = mean(lifeExp)),aes(x = year, y = meanLifeExp,color = continent)) +ylim(0,80)+geom_line()
combined <- ggarrange(A, B, labels=c("Scatter plot", "Line plot"))
combined

#Task B3
#Some plots of my creativity 
#Subset gapminder data, population in Afgainstan
ggplot(subset(gapminder, country == "Afghanistan"),
       aes(x = year, y = pop)) + geom_line() + geom_point()

#count how many passager survived in titanic (class depency)
titanic <- read.csv2('/Users/Lisa/Downloads/Telegram Desktop/titanic.csv')
ggplot(titanic, aes(x=pclass, color=factor(survived))) + geom_bar()
#gender
ggplot(titanic,aes( x=survived, fill=factor(gender))) + geom_bar()
#Some plots using airqality data, ozon level 
barplot(airquality$Ozone, xlab= " Ozone Concetrantion", ylab= "No of Instances", col= " red" , horiz = T)
