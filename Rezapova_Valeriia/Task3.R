f1 <- function(df,r,kol, fun, spl){
  sub_data_base <- subset(df, select=c(kol))[r,,drop=FALSE] 
  list1 <- list(subset=sub_data_base)
  fact <- as.factor(sub_data_base[,spl]) #a bit cheating, but it is user problem what to use for split
  list1[[2]] <- sapply(split(sub_data_base[,!names(sub_data_base)%in% c(spl)], fact, drop=FALSE), fun)
  #it is not a hardcore cause by this sub_data_base[,!names(sub_data_base)%in% c(spl)] I try to not count split col
  sub_data_base_chr <- sub_data_base[, sapply(sub_data_base, function(x) !is.numeric(x))]
  if (length(sub_data_base_chr)!=0){
    list1[[3]] <- sapply(sub_data_base_chr,table)
  }
  #All I want here is not make a table if I haven't got any non numeric values
  return(list1)
}
f1(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("disp","hp","carb"),colSums,'hp')
f1(iris,c(4,5),c(3,4,5), colMeans,'Species')
f1(airquality,c(1,2,3),c(TRUE,F,F,T,T,F), colMeans, 'Month')
