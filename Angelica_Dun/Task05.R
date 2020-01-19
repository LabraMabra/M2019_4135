ibrary(dplyr) 

data_processing <- function(data_frame, row_selection , column_selection, calc_func, split_f){
  
  
  #subsetting
  selected <- data_frame %>%  select(column_selection) %>% slice(row_selection)
  result <- list(selected)
  #select numeric data
  nums <- selected %>% lapply(is.numeric) %>% unlist()
  #create freq table for non-numeric
  freq <- selected[,!nums] %>% table()
  result <- c(result,freq)
  #split the original df
  fact_or <- split_f %>%  grep(data_frame %>% colnames())
  split_df <- selected %>% split( selected[,fact_or])
  #perform calculations using anonymnous function
  res <- split_df %>% lapply(function(x) {calc_func(x[nums])})
  result <- c(result, res)
  
  return(result)
}
print(data_processing(iris, 1:150, 1:5, mean, 'Species'))
