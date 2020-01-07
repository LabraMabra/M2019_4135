library(dplyr)
cars_data <- mtcars
my_function <- function(mtcars,row,col, math, spliting) {
  subsetting <- mtcars %>% select(col) %>% slice(row)
  fact <- as.factor(subsetting[,spliting])
  check_numeric <- dplyr::select_if(subsetting, is.numeric)
  split_for_digits <- lapply(split(subsetting[,!names(subsetting) %in% c(spliting)], fact), math)
  result <- list(subsetting,split_for_digits)
  return (result )
} 

my_function(mtcars, 1:10, c("hp","carb","disp"),colSums,"disp")
