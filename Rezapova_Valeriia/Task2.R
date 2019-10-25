f1 <- function(df,r,kol){
  sub_data_base <- subset(df, select=c(kol))[r,,drop=FALSE] #changed cause ncol(mtcars[10,10])=NULL
  list1 <- list(subset=sub_data_base)
  for (i in (1:ncol(sub_data_base))) {
    if(is.numeric(sub_data_base[,i])){
      list1[[length(list1)+1]] <- list(sum=sum(sub_data_base[,i]),mean=mean(sub_data_base[,i])) #I'm not sure
      }
    else{
      list1[[length(list1)+1]] <- list(table=table(sub_data_base[,i]))
    }
  }
  
  return(list1)
}
f1(mtcars,c("Mazda RX4","Mazda RX4 Wag"),c("disp","hp"))
f1(iris,c(4,5),c(4,5))
f1(airquality,c(1,2,3),c(TRUE,F,F,T,T,F))
f1(mtcars,10,10)
data <- mtcars
data$gear <- as.factor(data$gear)
f1(data,1:10,10:11) #here we have gear as factor and sum with mean cannot be applied on factors,
#so we get table. I'm not sure that i really have to change something cause else includes factors and characters

