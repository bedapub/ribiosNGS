#' Trim leading and tailing spaces from string
#' 
#' The function trims leading and/or tailing spaces from string(s), using C
#' function implemented in the BIOS library.
#' 
#' \code{left} and \code{right} can be set to NULL. In such cases no trimming
#' will be performed.
#' 
#' @param x A character string, or a vector of strings
#' @param left Characters that are trimmed from the left side.
#' @param right Characters that are trimmed from the right side
#' @return Trimmed string(s)
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @references
#' \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/format.hdoc}
#' @examples
#' 
#' myStrings <- c("This is a fine day\n",
#'                " Hallo Professor!",
#'                "  NUR DER HSV  ")
#' trim(myStrings)
#' 
#' @export trim
trim <- function(x, left=" \n\r\t", right=" \n\r\t") {
    if(is.null(left)) left <- ""
  if(is.null(right)) right <- ""
  .Call("trim", as.character(x), as.character(left), as.character(right))
}
