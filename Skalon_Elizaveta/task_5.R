library(dplyr)

# func to calculate mean
func_mean <- function(x){
  if (is.numeric(x)==TRUE){
    return(mean(x))
  } else{
    return(table(x))
  }
}

# the main function
# split_arg = column name

selection_looping <- function(df, x1, x2, split_arg, func_mean){
  df %>%
    slice(x1) %>%
    select(x2) -> selected_df
  
  selected_df %>%
    split(split_arg[x1], drop=T) %>%
    lapply(function(x){sapply(x, func_mean)} ) -> res
  
  returned <- list("df" = selected_df, "mean" = res)
  return(returned)
}

result <- selection_looping(iris, 97:102, 1:5, iris$Species, func_mean)

