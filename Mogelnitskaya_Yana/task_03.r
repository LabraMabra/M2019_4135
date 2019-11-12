process_func <- function(x, cfunc) 
    { if (is.numeric(x)) {
        return (do.call(cfunc, list(as.matrix(x))))}
    else {
        return (summary(x))}
    }
    
result_function <- function(xfull,row_criteria, col_criteria, split_criteria, cfunc) # where split_criteria is set as 'desirable column in quotes'
    {
    xf <- subset(xfull, select = c(col_criteria))[row_criteria, ,drop=FALSE]
    spcr <- as.factor(xf[,split_criteria])
    ls_1 <- lapply(split(xf, spcr, drop =TRUE), function(x){ lapply( x, process_func, cfunc) })
    ls <- list(xf, ls_1)
    return (ls)
}

print(result_function(iris, 5:10, 2:5,'Species', colSums))
print(result_function(iris, 5:10, 2:5,'Species', sum))
