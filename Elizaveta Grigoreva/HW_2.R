#Load different datasets and use one of them 
data1 <- iris
data2 <- cars
data3 <- airquality

#Create subsetting function that will take 3 arguments:dataframe, rows, columns
subsetting  <- function(data2, x, y){
#We need to keep result in the list, create variable result and indicate it as a list  
  result <- list()
#Create a variable 'small set or sub_set' that will be contain data, columns and rows
  small_set <- data2[x, y]
#Assign result variable subsetting function and save it into small set
  result$subset <- small_set
#Start loop to check if columns contain  numeric or non-numerical data. For each element in variable small set
  for (elements in names(small_set)) {
#Write result into variable columns_for_check ( [[for take more than elements]])
    columns_for_check <- small_set[[elements]]
#Write result of is it numeric or nor inside new variable 
    checking <- is.numeric(columns_for_check)
#If variable checking is numeric do some mathematics computations  (sqrt)   
    if (checking) {
#Write this into variable result 
      result[[elements]] <- sqrt(columns_for_check)
    } else {
#If it non-numeric data write it into table      
      result[[elements]] <- table(columns_for_check, dnn = NULL)
#Closeclose function return result as a list and close finally      
    }
  }
 return(result)
}

#Just for checking
check <- subsetting(data2,5:5,2:1)
check
