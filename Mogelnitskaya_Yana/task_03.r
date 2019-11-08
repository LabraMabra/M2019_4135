            f <- function(x) 
    {
    if (is.numeric(x)) {
            g <- sum(x)}
    else {
            g <- summary(x)
            }
    return (g)
    }
    
aapp <- function(xfull,row_criteria, col_criteria, split_criteria) # where split_criteria is set as 'desirable column in quotes'
    {
    xf <- subset(xfull, select = c(col_criteria))[row_criteria, ,drop=FALSE]
    spcr <- as.factor(xf[,split_criteria])
    ls_1 <- lapply(split(xf, spcr, drop =TRUE), function(x){sapply(x,f)})
    ls <- list(xf, ls_1)
    return (ls)
}

print(aapp(iris,5:10, 2:5,'Species'))
