  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(gapminder)
  #####Making iris graph###########
  iris_long <- iris  %>% mutate(id = 1:nrow(iris)) %>% gather(Part, Measure, 1:4 ) %>% 
    separate(Part, c('Part', 'Size_type'))  %>% spread(Size_type, Measure)
  ggplot(iris_long, aes(x=Length, y=Width, col =Part))+
    geom_point(size = 2)
  ######Gepminder#########
  #2007 year
  gapminder[gapminder$year == 2007,] %>% 
    ggplot(aes(x=gdpPercap, y =lifeExp, col = continent, size =pop))+
    geom_point()+
    scale_x_log10()
  #mean life expectancy
    gapminder %>% group_by(continent, year) %>% summarise(MeanLifExp = mean(lifeExp)) %>%
      ggplot(aes(x=year, y = MeanLifExp, col = continent ))+
        geom_point()+
        geom_smooth(method = "lm", se = FALSE)+
        coord_cartesian(ylim = c(0, 80))
  #total population over years
    gapminder %>% group_by(continent, year) %>% summarise(total_pop = sum(as.numeric(pop))) %>%
      ggplot(aes(x=year, y = total_pop, col = continent ))+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE)
   gapminder[gapminder$country =='Russia',]
  #bar_chart
   gapminder[gapminder$country %in% c('China','Japan'),] %>% 
     ggplot(aes(x= year, y = lifeExp, fill = country))+
      geom_bar(stat ='identity', position ='dodge')
  #facets
     gapminder %>% 
       ggplot(aes(x=gdpPercap, y =lifeExp, col = continent, size =pop))+
       geom_point()+
       facet_wrap(year~., scales = 'free')+
       scale_x_log10()+
     theme(axis.text.x=element_blank())
  #airquality
     airquality %>% gather(Parameter, Value, 1:4) %>%
       ggplot(aes(x=Day, y=Value, col = Parameter))+
       facet_grid(Parameter~Month, scales = 'free')+
       geom_line()
  #continuous distribution
     airquality %>% drop_na() %>% gather(Parameter, Value, 1:4) %>%
       ggplot(aes(Parameter,Value, fill = Parameter))+
       facet_wrap(Parameter~., scales = 'free')+
       geom_violin(show.legend = F, alpha=0.5, aes(col=Parameter))+
     theme(axis.text.x=element_blank())
     