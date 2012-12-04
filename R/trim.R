trim <- function(x, left=" \n\r\t", right=" \n\r\t") {
    if(is.null(left)) left <- ""
  if(is.null(right)) right <- ""
  .Call("trim", as.character(x), as.character(left), as.character(right))
}
