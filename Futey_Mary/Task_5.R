
#I was told by Valeria that we do not need to do a data type check for this task

library(dplyr)
library(tidyverse)

#filter rows, select columns, group by species, apply function, arrange
iris %>% filter(Sepal.Length < 5) %>% 
  select(starts_with("S")) %>% 
  group_by(Species) %>% 
  summarize(Sepal.Length = mean(Sepal.Length), Sepal.Width = mean(Sepal.Width)) %>% 
  arrange(desc(Sepal.Length))
