#!/usr/bin/R

choose_data <- function(data,col_sel,row_sel,fun_for_numeric = mean) {
  selected_data <- data[row_sel,col_sel]
  # make slice before evaluating
  lst <- list(selected_data)
  counter <- 2 # index for lst
  for (variable in selected_data) {
    if (is.numeric(variable)) {
      
      lst[[counter]] <- fun_for_numeric(variable)
      
    } else {
      lst[[counter]] <- table(variable)
    }
    
  counter <- counter + 1 
  # change counter to escape replace previous results.
  
  }
  
return(lst)
}

choose_data(iris[c(1,3,5,7,9),c(2,4,5)]) # lst[[1]] - 3.32, lst[[2]] - 0.22,
                                         # lst[[3]]: setosa - 5, versicolor - 0, virginica - 0
