data_processing <- function(data_frame, row_selection, column_selection, splitter, calculation_function) {
  subset <- data_frame[row_selection, column_selection, drop = F]
  
  calculation <- function(column, calculation_function) {
    if (is.numeric(column)) {
      do.call(calculation_function, list(column))
    } else {
      table(column)
    }
  }
  
  split_factor <- as.factor(subset[,splitter])
  # function(category) applies to each part of split subset
  calculations <- lapply(split(subset, split_factor, drop = T), function(category) {
    # calculation applies to each column of category
    lapply(category, calculation, calculation_function)
  })
  
  return(list(subset, calculations))
}


print(data_processing(iris, 1:100, 2:5, 'Species', sd))
print(data_processing(PlantGrowth, 1:20, c(T, T), 'group', mean))