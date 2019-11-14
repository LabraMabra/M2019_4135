apply_task <- function(df, row_sel, col_sel, oper, split_criteria){
  df_subset <- as.data.frame(df[row_sel,col_sel])
  fact <- as.factor(df_subset[,split_criteria])
  operation <- lapply(split(df_subset[,!names(df_subset) %in% c(split_criteria)], fact), oper) 
  data_check <- sapply(df_subset, function(x) if(!is.numeric(x)) table(x))
  if_null <- data_check[!sapply(data_check, is.null)]
  result <- list(df_subset,operation,if_null)
  return(result)
}

apply_task(iris,c(1,2,3,4,51,52,53,54,55,56,104,108,109),c(2,3,4,5), colMeans,'Species')
apply_task(airquality, c(1,2,3,4),c(3,4,5,6), colMeans, "Day")
