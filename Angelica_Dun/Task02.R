data_processing <- function(data_frame, row_selection, column_selection){
  selected <- data_frame[row_selection, column_selection]
  calc <-  list() 
  for (i in (1:ncol(selected))){
    if (is.numeric(selected[,i]) ){
      calc[[i]] <- c(mean(selected[,i])) 
    } else {
      calc[[i+1]] <-(table(selected[,i]))
    }
  }
  return(list(calc,selected))
}

print(data_processing(iris,1:100,1:5))
print(data_processing(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("disp","hp")))
print(data_processing(airquality,c(2,3,5),c(TRUE,F,F,T,T,F)))
print(data_processing(iris,c(1,5),c(1,5)))
