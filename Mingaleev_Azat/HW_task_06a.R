library(ggplot2)
library(tidyr)
iris_len <- gather(iris[,c(1,3,5)], Part, Length, -Species)
# aggregate Length parameter
iris_wid <- gather(iris[,c(2,4,5)], Part, Width, -Species)
# aggregate Width parameter
Width <- iris_wid$Width
# Prepare variable for new dataframe long_iris
long_iris <- cbind(iris_len, Width) %>% separate(Part, c("Part","Dummy"))
long_iris$Dummy <- NULL
ggplot(long_iris, aes(x = Length,
                      y = Width,
                      color = Species)) + geom_point(size = 1) + facet_grid(. ~Part)

