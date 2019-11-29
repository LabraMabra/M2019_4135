#function works only for datasets with both numeric and non-numeric colomns.
library(dplyr)
my_func <- function(dataset,row,col, math) {
  newdata <- dataset %>%  select(col) %>% slice(row) 
  calculation <- newdata %>% summarise_if(is.numeric, math, na.rm = TRUE)
  tabl <- newdata %>% select_if(funs(!is.numeric(.))) %>% table
  itog <- list(newdata,calculation,tabl)
  return (itog)
}

my_func(iris,c(2,3,4,5), c(2,3,4,5), mean)
my_func(PlantGrowth,c(10,11,12,13,21,22,23,24,25),c(1,2),sum)
