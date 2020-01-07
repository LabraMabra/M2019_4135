library(dplyr)

transform_col <- function(column, calc_func) {
  if (is.numeric(column)) {
    return(lapply(list(as.matrix(column)), calc_func))
  } else {
    return(table(column))
  }
}


df_subset <- function(dataf, row_select, col_select, split_on, calc_func) {
  subset_df <- dataf %>%
    select(col_select) %>%
    slice(row_select)
  
  transformations <- subset_df %>% 
    split(as.factor(subset_df[,split_on]), drop=TRUE) %>%
    lapply(function(column) {
      lapply(column, transform_col, calc_func)})
  return(list(subset_df, transformations))
}

print(df_subset(mtcars, 1:10, c("mpg", "drat", "gear"), "gear", mean))
