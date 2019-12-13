  data_process <- function(datafr, vector_rows , vector_columns, operation, split_f){
    
    
    #subset by rows and columms
    subset_1 <- as.data.frame(datafr[vector_rows,vector_columns])
    result <- list(subset_1)
    
    #select numeric columns
    nums <- unlist(lapply(subset_1, is.numeric)) 
    
    #make a frequency table for non numeric columns, before splitting the df,
    #because a freq table of a df split by species doesnt seem useful to me
    tables_nonnumeric <- table(subset_1[,!nums])
    result <-c(result,tables_nonnumeric)
    
    #gets index of column to use as split factor from column label
    fact <- grep(split_f , colnames(datafr))
    
    #split df
    split_df <- split(subset_1, subset_1[,fact])
    
    
    #apply chosen calculation only to numerical 
    result_numeric <- lapply(split_df, function(x) {operation(x[nums])})
    result <-c(result, result_numeric)      
    
    
    
    return(result)
  }
  
  print(data_process(iris,c(1:100),c(1:5), colSums, "Species"))
