library(dplyr)
#To be honest, I don't know what to change to use pipes
f1 <- function(kol,fun){
  if (is.numeric(kol)){
    lapply(list(kol),fun)
  }
  else{
    table(kol)
  }
}

f2 <- function(df,r,kol, fun, spl){
  sub_data_base <- df%>%select(kol)
  sub_data_base <- sub_data_base%>%slice(r)
  splitted <- lapply(split(sub_data_base,spl, drop=FALSE), function(kol){
    lapply(kol, f1, fun)})
  return(list(sub_data_base,splitted))
}
f2(mtcars,c(1,2),c("disp","hp","carb"),sum,'hp')
f2(iris,c(4,5),c(3,4,5), mean,'Species')
f2(airquality,c(1,2,3),c(1,4,5), mean, 'Month')
