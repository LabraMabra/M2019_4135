data_process <- function(datafr, vector_rows , vector_columns){
  subset_1 <- data.frame(datafr[vector_rows,vector_columns])
  result <- list(subset_1)
  for (i in (1:ncol(subset_1))){
    if (is.numeric(subset_1[,i]) ){
      result <- c(result, mean(subset_1[,i])) }
    if (is.numeric(subset_1[,i]) == FALSE){
      result[[i+1]] <-(table(subset_1[,i]))
    }
  }
  return(result)
}

print(data_process(iris,c(1:100),c(1:5)))

print(data_process(airquality,c(1,3),c(TRUE,F,T,T,T,F)))

print(data_process(cars,c(1:40),c("speed","dist")))

print(data_process(iris,c(1:100),1))

print(data_process(iris,1,1))
