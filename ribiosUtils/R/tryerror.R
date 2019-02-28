#' Tell whether an object is an error
#' 
#' Determines whether an object is of class \code{try-error}
#' 
#' 
#' @param x Any object, potentially produced within a \code{try-error}
#' structure.
#' @return Logical value, \code{TRUE} if \code{x} inherits the \code{try-error}
#' class.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' if(exists("nonExistObj")) rm(nonExistsObj)
#' myObj <- try(nonExistObj/5, silent=TRUE)
#' isError(myObj)
#' 
#' @export isError
isError <- function(x) {
  inherits(x, "try-error")
}
