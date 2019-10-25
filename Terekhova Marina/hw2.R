
my_func <- function(dataset,row,col, math, splitparametr) {
  subsety <- as.data.frame(dataset[row,col])
  fact <- as.factor(subsety[,splitparametr])
  splitovan_chisl <- lapply(split(subsety[,!names(subsety) %in% c(splitparametr)], fact), math)
  nechislovy <- sapply(subsety, function(x) if (!is.numeric(x)) table(x))
  beznull <- nechislovy[!sapply(nechislovy, is.null)]
  itog <- list(subsety,splitovan_chisl,beznull)
  return (itog)
} 

my_func(PlantGrowth,c(10,11,12,13,21,22,23,24,25),c(1,2),sum, 'group')
my_func(iris,c(1,2,3,4,51,52,53,54,55,56,104,108,109),c(2,3,4,5),sum, 'Species')
my_func(mtcars,c(1,2,3,4,10,11,12,13,21,22,23,24),c(2,3,4,5),sum, 'cyl')


