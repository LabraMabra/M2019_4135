func <- function(df, x1, y1, x2, y2){
  new_df <- df[x1:y1, x2:y2]
  mean_list <- list()
  freq_list <- list()
  for (i in 1:ncol(new_df)){
    if (is.numeric(new_df[,i])==TRUE){
      mean_list[[names(df)[i]]] <- mean(new_df[,i])
      i <- i+1
    } 
    else {
      freq_list[[names(df)[i]]] <- table(new_df[,i])
      i <- i+1
    }
  }
  ret <- list("df" = new_df, "mean" = mean_list, "frequency" = freq_list)
  return(ret)
}

df <- iris
out_iris<- func(df, 1, 10, 1, 5)
out_iris$df
out_iris$mean
out_iris$frequency

df1 <- Theoph
out_Theoph <- func(df1, 1, 20, 1, 4)
out_Theoph$df1
out_Theoph$mean
out_Theoph$frequency