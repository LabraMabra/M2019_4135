
library(datasets)
data("iris")
summary(iris)





foo <- function(data, rows, cols) {
  res <- list()
  sub_set <- data.frame(data[rows, cols])
  res$subset <- sub_set

  for (col_name in names(sub_set)) {
    col <- sub_set[[col_name]]
    type_check <- is.numeric(col)

    if (type_check) {
      res[[col_name]] <- mean(col)
    } else {
      res[[col_name]] <- table(col, dnn = NULL)
    }
  }
  res
}


foo(iris, 1:5, 1:5)
foo(iris, 1:5, 1)




# foo_2 <- function(data, rows, cols) {
#   res <- list()
  
#   sub_set <- data[rows, cols]
#   res$subset <- sub_set
  
#   for (col_name in names(sub_set)) {
#     col <- sub_set[[col_name]]
#     type_check <- is.character(col)
    
#     if (type_check) {
#       res[[col_name]] <- table(col, dnn = NULL)
#     } else {
#       res[[col_name]] <- sum(col)
#     }
#   }
#   res
# }

# foo_2(airquality, 1:2, 1:5)


  




