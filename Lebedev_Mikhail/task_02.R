
df_subset <- function(dataf, row_select, col_select) {
  subset_df <- dataf[row_select, col_select, drop = FALSE]
  l <- list(subset_df)
  for (i in subset_df) {
    if (is.numeric(i)) {
      l <- c(l, sd(i))
    } else {
      l <- c(l, table(i))
    }
  }
  return(l)
}

print(df_subset(mtcars, 1:10, c("mpg", "drat")))
print(df_subset(mtcars, c("Merc 240D", "Hornet 4 Drive"), c(1,5)))
print(df_subset(iris, c(T, F, F), "Species"))
