data_process <- function(data_frame, vector_1 , vector_2){
  subset_1 <- data_frame[vector_1,vector_2]
  result = list()
  result = c(result,subset_1)
  for (i in (1:ncol(subset_1))){
    if (is.numeric(subset_1[,i]) == TRUE){
      result <- c(result, mean(subset_1[,i])) }
    if (is.numeric(subset_1[,i]) == FALSE){
      abx <- subset_1[,i]
      freq <- table(abx)
      result <-c(result, freq)
    }
  }
  return(result)
}

print(data_process(iris,c(1:100),c(1:5)))

print(data_process(airquality,c(1,3),c(TRUE,F,T,T,T,F)))

print(data_process(cars,c(1:40),c("speed","dist")))

