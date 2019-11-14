subsetting  <- function(data2, x, y){
  result <- list()
  small_set <- subset(data2, select=c(x))[y, ,drop=FALSE]
  #Assign result variable subsetting function and save it into small set
  result$subset <- small_set
  #Start loop to check if columns contain  numeric or non-numerical data. For each element in variable small set. 
  for (elements in names(small_set)) {
    columns_for_check <- small_set[[elements]]
    checking <- is.numeric(columns_for_check)
    if (checking) {
      result[[elements]] <- sqrt(columns_for_check)
    } else {    
      result[[elements]] <- table(columns_for_check, dnn = NULL)     
    }
  }
  return(result)
}

#Just for checking
check <- subsetting(mtcars,5:5,2:1)
check

data1 <- iris
data2 <- mtcars
data3 <- airquality
