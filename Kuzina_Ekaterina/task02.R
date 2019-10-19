df_subset <- function(data_frame, rows, columns){
  df <- data.frame(data_frame[rows, columns])
  result <- list(df)
  for (i in c(1:ncol(df))) {
    if (is.numeric(df[, i])) {
      result[[i+1]] <- mean(df[, i])
    }
    else {
      result[[i+1]] <- table(df[, i])
    }
  }
  return(result)
}

df <- iris
print(df_subset(df, c(49:52), c('Petal.Length', 'Species')))

df <- mtcars
print(df_subset(df, c(F, F, T), c('cyl', 'gear', 'carb')))

print(df_subset(df, 1, 2))


