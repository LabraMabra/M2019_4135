calculation <- function(column, calculation_function) {
  if (is.numeric(column)) {
    do.call(calculation_function, list(column))
  } else {
    table(column)
  }
}

data_processing <- function(data_frame, row_selection, column_selection, splitter, calculation_function) {
  data_frame %>%
    slice(row_selection) %>%
    select(column_selection) ->
    subset
  
  subset %>%
    split(as.factor(subset[,splitter]), drop = T) %>%
    lapply(function(category) {
      lapply(category, calculation, calculation_function)
    }) ->
    calculations
  
  return(list(subset, calculations))
}


print(data_processing(iris, 1:100, 3:5, 'Species', sd))
print(data_processing(PlantGrowth, 1:20, c(1, 2), 'group', mean))