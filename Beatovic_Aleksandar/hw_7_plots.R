ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() +
  facet_wrap(~year, nrow = 3, ncol = 4) + 
  scale_x_log10()

airquality %>% gather(Measure, Value, Ozone:Temp) %>% ggplot( aes(x = Day, y = Value, color = Measure)) + 
  facet_grid(Measure~Month, scales="free_y") + 
  geom_line() + geom_point(shape = 21, size = 2, fill = 'white')

airquality %>%  ggplot(aes(x = Wind, fill = as.factor(Month))) + geom_histogram(position = 'dodge', bins = 10)

