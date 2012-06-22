mmatch <- function(x, table, nomatch=NA_integer_) {
 .Call("mmatch",
               as.character(x), as.character(table), nomatch)
}
