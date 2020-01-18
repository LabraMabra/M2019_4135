library(dplyr)

manipulation <- function(row, mathfunc) {
  if (is.numeric(row)) {
    return(do.call(mathfunc, list(as.matrix(row))))
  } else {
    return(summary(row))
  }
}

data_manipulation <- function(data_frame, row_selection, colomn_selection, splt, mathfunc) {
  
  df <- data_frame[row_selection, colomn_selection]
  splt <- as.factor(df[,splt])
  
  df_n <-
    df %>%
    split(splt, drop = T) %>% lapply(function(x){lapply(x, manipulation, mathfunc)})
  
  return(list(df, df_n))
}

print(data_manipulation(iris, 1:150, 1:5,'Species', colMeans))

print(data_manipulation(cars, 1:50, 1:2, 'speed', colSums))
