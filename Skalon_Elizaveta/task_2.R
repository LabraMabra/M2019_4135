selection <- function(df, x1, x2){
  selected_df <- df[x1, x2]
  mean_list <- list()
  freq_list <- list()
  # for 1-column situations:
  if (is.null(ncol(selected_df))==TRUE){
    selected_df <-data.frame(selected_df)
  }
  for (i in 1:ncol(selected_df)){
    if (is.numeric(selected_df[,i])==TRUE){
      mean_list[[names(df)[i]]] <- mean(selected_df[,i])
      i <- i+1
    } 
    else {
      freq_list[[names(df)[i]]] <- table(selected_df[,i])
      i <- i+1
    }
  }
  returned <- list("df" = selected_df, "mean" = mean_list, "frequency" = freq_list)
  return(returned)
}

df <- iris
index_row <- 1:10
index_col <- 1:5
out_iris<- selection(df, index_row, index_col)
out_iris$df
out_iris$mean
out_iris$frequency

df <- Theoph
index_row <- 1:20
index_col <- 1:4
out_Theoph <- selection(df, index_row, index_col)
out_Theoph$df
out_Theoph$mean
out_Theoph$frequency

