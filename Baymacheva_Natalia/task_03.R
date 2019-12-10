f <- function(data_frame, row_selection, colomn_selection, splt, mathfunc) {
  
  #new dataframe
  df <- data_frame[row_selection, colomn_selection]
  
  #splitting the dataframe:
  # 1.finding the responding column. 2.and then splitting by that factor 
  df_split <- split(df, df[,grep(splt, colnames(df))])
  
  #finding numeric columns:
  numcols <- sapply(df, is.numeric)
  
  #freq table for not numeric:
  df_table <- table(df[,!numcols])
  
  #math operation on numeric data:
  df_math <- lapply(df[,numcols], mathfunc)
  
  return(list(df_split, df_table, df_math))
  
}

f(iris, 1:150, 1:5, 'Species', mean)
f(PlantGrowth,1:30,c(1,2), 'group', mean)