apply_task <- function(df, row_sel, col_sel, oper){
  df_subset <- data.frame(df[row_sel,col_sel])
  result <- lapply(df,function(x){sapply(x, f)})
  return(result)
}

apply_task(iris, c(1:5), 1, mean)