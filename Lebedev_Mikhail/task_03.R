transform_col <- function(column, calc_func) {
  if (is.numeric(column)) {
    lapply(list(column), calc_func)
  } else {
    table(column)
  }
}


df_subset <- function(dataf, row_select, col_select, split_on, calc_func) {
  subset_df <- dataf[row_select, col_select, drop = FALSE]
  transformations <- lapply(split(subset_df, split_on, drop = FALSE), function(column) {
    lapply(column, transform_col, calc_func)
  })
  return(list(subset_df, transformations))
}


print(df_subset(mtcars, 1:10, c("mpg", "drat"), mtcars$gear, mean))
