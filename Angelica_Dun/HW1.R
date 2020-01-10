hw1 <- function(data_frame, row_selection, column_selection){
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

print(hw1(iris,c(1:100),c(1:5)))
print(hw1(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("disp","hp")))
print(hw1(airquality,c(2,3,5),c(TRUE,F,F,T,T,F)))
print(hw1(iris,c(1,5),c(1,5)))