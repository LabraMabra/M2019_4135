cars_data <- cars
#Identify function and subsetting, check numeric and non-numeric data, remove zeros, merge and return result
my_function <- function(cars,row,col, math, spliting) {
  subsetting <- as.data.frame(cars[row,col])
  fact <- as.factor(subsetting[,spliting])
  #changed here
  check_numeric <- lapply(subsetting, function(x) if (!is.numeric(x)) table(x))
  remove_zero <- check_numeric[!sapply(check_numeric, is.null)]
  split_for_digits <- lapply(split(subsetting[,!names(subsetting) %in% c(spliting)], fact), math)
  result <- list(subsetting,split_for_digits,remove_zero)
  return (result )
} 


my_function(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("hp","carb","disp"),colSums,"disp")

