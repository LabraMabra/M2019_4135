f1 <- function(df,r,kol){
  list1 <- list()
  list2 <- list()
  v_sum <- c()
  v_mean <- c()
  data_base <- df
  sub_data_base <- data_base[r,kol]
  list1 <- list(as.matrix(sub_data_base))
  for (i in (1:ncol(sub_data_base))) {
    if(is.numeric(sub_data_base[,i])==TRUE){
      v_sum[i] <- sum(sub_data_base[,i])
      v_mean[i] <- mean(sub_data_base[,i])}
    else{
      list2 <- append(list2,list(table(sub_data_base[,i])))
    }
  }
  list1 <- append(list1,list(t(as.matrix(v_sum))))
  list1 <- append(list1,list(t(as.matrix(v_mean))))
  list1 <- append(list1,list2)
  list1 <- list1[lengths(list1)>0L]
  return(list1)
}
f1(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("disp","hp"))
f1(iris,c(4,5),c(4,5))
f1(airquality,c(1,2,3),c(TRUE,F,F,T,T,F))
