
subs <- function(data, row_selector, column_selector, split_selector) {
result <- subset(data, select = c(column_selector))[row_selector, ,drop=FALSE] %>% 
  split(subset(data, select = c(split_selector))[row_selector,] , drop =TRUE)

c(list(subset(data, select = c(column_selector))[row_selector, ,drop=FALSE]),
list(mean = lapply(result, function(x) sapply(x,function(x) ifelse(is.numeric(x), mean(x), table(x)))), 
 sum = lapply(result, function(x) sapply(x,function(x) ifelse(is.numeric(x), sum(x), table(x)))), 
sd = lapply(result, function(x) sapply(x,function(x) ifelse(is.numeric(x), sd(x), table(x))))) ) %>% return()
}

print(subs(mtcars,1:10,1:4,2))

