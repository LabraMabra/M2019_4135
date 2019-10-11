city <- c("City1", "City1", "City2", "City2", "City3", "City3")
sex <- c("Male", "Female", "Male", "Female", "Male", "Female")
number <- c(12450, 10345, 5670, 5800, 25129, 26000)
CITY <- data.frame(City = city, Sex = sex, Number = number) 


my_func <- function(x,y,z) {
  itog <-list()
  sor <- as.data.frame(x[y,z])
  for (i in 1:ncol(sor)) {
    colomn <- sor[[i]]
    numcol <-is.numeric(colomn)
    if (numcol) {
      itog[[i]] <- sum(colomn)
    } else {
      itog[[i]] <- table(colomn)
    } 
  }
  return (itog2 <- list(sor,itog))
}


my_func(CITY,c(2,3,4),c(2,3))
my_func(mtcars,c(2,6,8),c(1,6))