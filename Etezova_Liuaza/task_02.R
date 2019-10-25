data_processing <- function(data_frame, row_selection, column_selection) {
  subset <- data_frame[row_selection, column_selection, drop = FALSE]
  calculations <- list()
  i <- 1

  for (column in subset) {
    if (is.numeric(column)) {
      calculations[[i]] <- sum(column)
    } else {
      calculations[[i]] <- table(column)
    }
    i <- i + 1
  }
  
  return(list(subset, calculations))
}

print(data_processing(iris, 1:3, 1:2))
print(data_processing(mtcars, c("Mazda RX4", "Valiant"), c("disp", "qsec", "vs")))
print(data_processing(PlantGrowth, 1:10, 1:2))
print(data_processing(PlantGrowth, 10:12, c(T, F)))