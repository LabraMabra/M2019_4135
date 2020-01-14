library(dplyr) 

data_process <- function(datafr, vector_rows , vector_columns, operation, split_f){
  
  
  #subset by rows and columms
  subset_1 <- datafr %>%  select(vector_columns) %>% slice(vector_rows)
  result <- list(subset_1)
  
  #select numeric columns
  nums <- subset_1 %>% lapply(is.numeric) %>% unlist()
  
  #make a frequency table for non numeric columns, before splitting the df,
  #because a freq table of a df split by species doesnt seem useful to me
  tables_nonnumeric <- subset_1[,!nums] %>% table()
  result <-c(result,tables_nonnumeric)
  
  #gets index of column to use as split factor from column label
  fact <- split_f %>%  grep(datafr %>% colnames())
  
  #split df
  split_df <- subset_1 %>% split( subset_1[,fact])
  
  
  #apply chosen calculation only to numerical 
  result_numeric <- split_df %>% lapply(function(x) {operation(x[nums])})
  result <-c(result, result_numeric)      
  
  
  
  return(result)
}

print(data_process(iris,c(1:100), c(1:5), colSums, "Species"))
