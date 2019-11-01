# the calculation function as a formal argument
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
  selected_df <- df[x1, x2]
  # for 1-column situations:
  if (is.null(ncol(selected_df))==TRUE){
    selected_df <-data.frame(selected_df)
  }
  splitted_df <- split(selected_df, split_arg[x1], drop=T)
  res <- lapply(splitted_df, function(x){sapply(x, func_mean)} )
  returned <- list("df" = selected_df, "mean" = res)
  return(returned)
}

result <- selection_looping(iris, 97:102, 1:5, iris$Species, func_mean)
