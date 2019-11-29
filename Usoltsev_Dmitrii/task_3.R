summary_ <- function(x) {
if (is.numeric(x)) {
act <- sum(x) }
else {
act <- summary(x) }
return (act)
}

mean_ <- function(x) {
  if (is.numeric(x)) {
    act <- mean(x) }
  else {
    act <- summary(x) }
  return (act)
}

sd_ <- function(x) {
  if (is.numeric(x)) {
    act <- sd(x) }
  else {
    act <- summary(x) }
  return (act)
}

sap_s <- function(x) {
  sapply(x,summary_)
}

sap_m <- function(x) {
  sapply(x,mean_)
}

sap_sd <- function(x) {
  sapply(x,sd_)
}

subs <- function(data, row_selector, column_selector, split_selector) {
  sub <- subset(data, select = c(column_selector))[row_selector, ,drop=FALSE]
  sub1 <- subset(data, select = c(split_selector))[row_selector,]
  splitter <- split(sub, sub1, drop =TRUE)
  #result <- lapply(splitter, sap_s)
  M <- list(sub)
  M[[length(M)+1]] <-list(mean = lapply(splitter, sap_m), sum = lapply(splitter, sap_s), 
                          sd = lapply(splitter, sap_sd))
  return(M)
}
  
print(subs(mtcars,1:10,c(1,3),2))

