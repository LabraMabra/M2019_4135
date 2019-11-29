data_process <- function(datafr, vector_rows , vector_columns, operation, split_factor){
  subset_1 <- data.frame(datafr[vector_rows,vector_columns])
  result <- list(subset_1)

  analysis <- lapply((split(subset_1, spsplit_factor), function(x) if(is.numeric(x)){operation(x)} else table(x))
  result <-c(result,analysis)
  return(result)
}

print(data_process(iris,c(1:100),c(1:5), sum, iris$Species))

print(data_process(airquality,c(1,3),c(TRUE,F,T,T,T,F), mean, airquality$Ozone))

