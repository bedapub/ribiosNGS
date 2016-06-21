#' Print a decimal number in procent format
#'
#' @param x a decimal number, usually between -1 and 1
#' @param fmt format string, '1.1' means a digit before and after the decimal point will be printed
#'
#' @examples
#' procent(c(0,0.1,0.25,1))
#' procent(c(0,0.1,0.25,1), fmt="1.4")
#' procent(c(0,-0.1,0.25,-1), fmt="+1.1")
#'
#' @export
procent <- function(x, fmt="1.1") {
    format <- paste("%", fmt, "f%%", sep="")
    sprintf(format, x*100)
}
