# HW3 %>% HW5


# If you have factor variable as numbers,
# convert them to charater type or whatever approach
stat <- function(data=mtcars, cols=1:ncol(data), rows=1:nrow(data), num_f=median){
  
lapply(data %>% dplyr::select(cols) %>% dplyr::slice(rows), 
       function(x) ifelse(is.numeric(x),
                                num_f(x, na.rm = T),
                                chisq.test(table(x))
))

  }

stat()
