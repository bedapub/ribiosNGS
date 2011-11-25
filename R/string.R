nField <- function(str, split="\t",...)
  sapply(strsplit(str, split, ...), length)


## grep in the order of '|' separated keywords
orderedGrep <- function(...) {atomGrep(...)}
atomGrep <- function(pattern, str) {
    atom <- unlist(strsplit(pattern, "\\|"))
    unname(unlist(sapply(atom, grep, str)))
}

##test.atomGrep <- function() {
##  pattern <- c("Google|Yahoo|flickr|twitter")
##  str <- c("youtube","Google","I don't use twitter","facebook","gmx","I live on Google","twitter","Yahoo","MySpace")
##  checkEquals(atomGrep(pattern, str),c(2,6,8,3,7))
##}
