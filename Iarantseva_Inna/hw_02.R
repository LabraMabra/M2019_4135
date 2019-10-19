process_data_frame <- function(data_frame, rows, columns) {
  subset <- data_frame[rows, columns, drop=FALSE]
  result_list <- list()
  
  for (value in subset) {
    if (is.numeric(value)) {
      result_list <- c(result_list, sum(value))
    } else {
      result_list <- c(result_list, table(value))
    }
  }
  
  return(result_list)
}

process_data_frame(iris, 10:20, 4:5)
process_data_frame(airquality, 10:60, 3:5)