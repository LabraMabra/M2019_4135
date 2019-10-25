#!/usr/bin/R


check.type <- function(data,check.function=is.numeric) {
  
  check.res <- vector(length = length(data))
  # Create a logical vector with fixed length.
  # length is number of columns of our dataframe
  for (i in 1:length(data)) {
  
      check.res[i] <- is.numeric(data[,i])
  
  }
  
  return(check.res)
}




#check.type(iris)


choose_data2 <- function(data,col_sel,row_sel,fun_for_numeric = mean, na.rm = TRUE) {
  selected_data <- data[row_sel,col_sel]
  # make slice before evaluating
  numeric_data <- selected_data[,check.type(selected_data)]
  # aggregate data for numeric analysis
  categorical_data <- selected_data[,!check.type(selected_data)]
  # aggregate categorical data. In our case we call function table on this data
  
  if (sum(check.type(selected_data)) == length(check.type(selected_data))) {
  # if we have only numeric data
  lst <- list(selected_data,
                apply(numeric_data,2,fun_for_numeric,na.rm = TRUE))
  } else {
    lst <- list(selected_data,
                apply(numeric_data,2,fun_for_numeric,na.rm = TRUE),
                table(categorical_data))
    # If we have at least one column with categorical data,
    # we add result table(categorical_data)
     }
  
  
  
  
  return(lst) }

iris_test <- choose_data2(iris)
airquality_test <- choose_data2(airquality)
mtcars_test <- choose_data2(mtcars)
