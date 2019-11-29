library(dplyr)

process_func <- function(x, cfunc) 
{ if (is.numeric(x)) {
  return (do.call(cfunc, list(as.matrix(x))))}
  else {
    return (summary(x))}
}

result_function <- function(xfull,row_criteria, col_criteria, split_criteria, cfunc){
  xf <- xfull %>% 
          select(col_criteria) %>% 
          slice (row_criteria)
  spcr <- as.factor(xf[,split_criteria])
  ls_1 <- xf %>% 
            split(spcr, drop=TRUE) %>%
            lapply(function(x){lapply(x, process_func, cfunc)})
  ls <- list(xf, ls_1)
  return (ls)
}

print(result_function(iris, 5:10, 2:5,'Species', colMeans))
