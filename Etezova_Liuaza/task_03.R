data_processing <- function(data_frame, row_selection, column_selection, splitter, calculation_function) {
  subset <- data_frame[row_selection, column_selection, drop = F]
  
  # function(category) applies to each part of split subset
  calculations <- lapply(split(subset, splitter, drop = T), function(category) {
    # calculation applies to each column of category
    lapply(category, calculation, calculation_function)
  })
  
  calculation <- function(column, calculation_function) {
    if (is.numeric(column)) {
      do.call(calculation_function, list(column))
    } else {
      table(column)
    }
  }
  
  return(list(subset, calculations))
}


print(data_processing(iris, 1:150, 2:5, iris$Species, sd))
print(data_processing(PlantGrowth, 1:30, c(T, T), PlantGrowth$group, mean))