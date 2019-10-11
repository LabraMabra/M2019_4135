data_processing <- function(data_frame, row_selection, column_selection) {
  subset <- data_frame[row_selection, column_selection, drop=FALSE]
  processed_data <- list(subset)

  for (column in subset) {
    if (is.numeric(column)) {
      processed_data <- c(processed_data, sum(column))
    } else {
      processed_data <- c(processed_data, table(column))
    }
  }
  
  return(processed_data)
}

print(data_processing(iris, 1:3, 1:2))
print(data_processing(mtcars, c("Mazda RX4", "Valiant"), c("disp", "qsec", "vs")))
print(data_processing(PlantGrowth, 1:10, 1:2))
print(data_processing(PlantGrowth, 10:12, c(T, F)))
