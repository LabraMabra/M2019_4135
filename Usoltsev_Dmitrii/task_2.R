subs <- function(tab,x,y){ # x-row selector, y-column selector
  sub <- subset(tab, select = c(y))[x, ,drop=FALSE]
  M <- list(sub)
  for (i in 1:length(sub)) {
    if (is.numeric(sub[,i])) {
      M[[length(M)+1]] <-list(mean = mean(sub[,i]), sum = sum(sub[,i]), sd = sd(sub[,i]))
    }
    if (is.factor(sub[,i]) | is.logical(sub[,i]) | is.character(sub[,i])) {
      M[[length(M)+1]] <-list(table(sub[,i]))
    }
    
  }
  names(M) <- c('subset', names(sub))
  return(M)
}


str(iris)


subs(iris,c(1:10),1:2)

 