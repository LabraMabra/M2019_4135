data_process <- function(datafr, vector_rows , vector_columns, operation){
  subset_1 <- data.frame(datafr[vector_rows,vector_columns])
  result <- list(subset_1)
  analysis <- lapply(datafr, function(x) if(is.numeric(x)){operation(x)} else table(x))
  result <-c(result,analysis)
  return(result)
}

print(data_process(iris,c(1:100),c(1:5), sum))

print(data_process(airquality,c(1,3),c(TRUE,F,T,T,T,F), mean))

print(data_process(cars,c(1:40),c("speed","dist"), sum))

print(data_process(iris,c(1:100),1, sum))

print(data_process(iris,1,1, sum))

