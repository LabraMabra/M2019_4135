library(gapminder)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

#part1_A  - iris to iris_long

iris_long <- cbind(1:count(iris)$n, iris) %>%
  gather(Criterias, Value,Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  separate(Criterias, into = c("Part", "Measurement")) %>% 
  spread(Measurement, Value) %>% 
  select (-1)

iris_long %>%  
  ggplot(aes(x = Length, y = Width, fill = Part)) +
  geom_point(shape = 23, color = "black") +
  scale_fill_manual(values=c("yellow", "green")) # no need in this line, i was just intrested if i can change it manually 
  

#part1_B1 - gapminder[2007]

data <- filter(gapminder, year == 2007)

data %>%  
  ggplot(aes(x = gdpPercap, y =lifeExp, 
       color = continent,size = pop)) +
       geom_point(shape = 19)


#part1_B2 - scatter+line plot for meanlifeExp
data <- gapminder %>%  
          group_by(continent, year) %>%
          summarise(meanlifeExp = mean(lifeExp)) 

scatterPlot <- data %>%
      ggplot(aes( x = year, y = meanlifeExp, color = continent)) +
      geom_point(shape = 15) 

linePlot <-  data %>%
      ggplot(aes( x = year, y = meanlifeExp, color = continent)) +
      geom_line()

ggarrange(scatterPlot, linePlot, 
      labels = c("Scatter Plot", "Line Plot"),
      ncol = 2, nrow = 1)

#part1_B3 - any bar chart: world gdp over years 

data <- gapminder %>%  
      group_by(continent, year) %>%
      summarise(gdp = sum(gdpPercap*pop))

data %>% 
      ggplot(aes(x = year, y = gdp, fill = continent)) +
      geom_col() 

#part2_1 - gapminder, all years facet 
gapminder %>%
      ggplot(aes(x = gdpPercap, y =lifeExp, color = continent,size = pop)) +
      geom_point(shape = 19) +
      facet_wrap(~ year, nrow = 3, ncol = 4) +
      scale_x_log10() 

#part2_2 - airquallity 

data <- gather(airquality, Measurement, Value, Ozone, Solar.R, Temp, Wind )

data %>%  
      ggplot(aes(x = Day , y = Value, color = Measurement)) +
      geom_line() + geom_point(size = 1, shape = 19, na.rm = TRUE)+
      facet_grid( Measurement ~ Month, scales = "free_y") 
  
#part2_3 - distibutional plots 
#I used USArrests dataset, for data reshaping part I decided to assemble states into 4 regions

data <- cbind(rownames(USArrests),USArrests ) %>%
      gather(Crime, Affected, Murder, Assault, UrbanPop, Rape) 

colnames(data) <- c("State", "Crime", "Affected")
levels(data$State) <- cbind(levels(data$State), "Northeast", "Midwest", "West", "South")

data$State[which(data$State %in% c("Maine","New Hampshire", "Vermont", 
                             "Massachusetts", "Rhode Island", 
                             "Connecticut", "New York", 
                             "New Jersey","Pennsylvania"))] <- "Northeast"

data$State[which(data$State %in% c("Ohio", "Michigan", "Indiana", "Wisconsin", 
                              "Illinois", "Minnesota", "Iowa", "Missouri", 
                              "North Dakota", "South Dakota", "Nebraska", 
                              "Kansas"))] <- "Midwest"

data$State[which(data$State %in% c("Delaware", "Maryland", "Virginia",
                                  "West Virginia", "Kentucky", "North Carolina", 
                                  "South Carolina", "Tennessee", "Georgia", 
                                  "Florida", "Alabama", "Mississippi", "Arkansas", 
                                  "Louisiana", "Texas","Oklahoma"))] <- "South"

data$State[which(data$State %in% c("Montana", "Idaho", "Wyoming", 
                                  "Colorado", "New Mexico", "Arizona", 
                                  "Utah", "Nevada", "California", "Oregon", 
                                  "Washington", "Alaska","Hawaii"))] <- "West"

colnames(data) <- c("Region", "Crime", "Affected")

ggplot(data, aes(x = Affected , fill = Crime))+
      geom_density(alpha = 0.5) +
      facet_wrap(~ Region,  nrow = 2, ncol = 2, scales = "free_y")
