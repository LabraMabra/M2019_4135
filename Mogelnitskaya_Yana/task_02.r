hw <- function(xfull,row_criteria, col_criteria) {
    ls <- list()
    xf <- subset(xfull, subset = row_criteria, select = col_criteria)
    row.names(xf) <- c(1:nrow(xf))
    ls[[1]] <- xf
    n <-c("Subsetted data")
    for (i in 1:ncol(xf)) {
        if (is.numeric(xf[[i]][1])) {
            ls[[i+1]] <- sum(xf[i])
            n <- c(n,toString(c(names(xf[i])," sum")))}
    else {
          ls[[i+1]] <- summary(xf[i])
          n <- c(n, toString(c(names(xf[i])," frequency table")))}
    }
    names(ls) <- n
    return(ls)
    }

