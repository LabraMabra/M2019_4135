data_processing <- function(data_frame, row_selection, column_selection, splt, calc_func) {
  selected <- data_frame[row_selection, column_selection]
  #split the df:
  df_split <- split(data_frame, data_frame[,grep(splt, colnames(data_frame))])
  
  #find numeric data
  numcolumns <- sapply(selected, is.numeric)
  #calculations for numeric data as in task 02
  selected_calculated <- sapply(selected[,numcolumns], calc_func)
  
  #frequency table for non-numeric
  selected_freq <- table(selected[,!numcolumns])
  
  #for output: calculations, frequency table
  return(list(df_split, selected_calculated, selected_freq))
}

print(data_processing(iris, 1:150, 1:5, 'Species', mean))
print(data_processing(PlantGrowth, 1:25, c(T, T), 'group', mean))

  
