#' Keep silent by suppressing warnings and messages
#' 
#' The function is used to keep the command silent by suppressing warnings and
#' messages
#' 
#' 
#' @param \dots Any function call
#' @return The same as the function call
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{suppressWarnings}}, \code{\link{suppressMessages}}
#' @examples
#' 
#' wsqrt <- function(x) {warning("Beep");message("Calculating square");return(x^2)}
#' \dontrun{wsqrt(3)}
#' silencio(wsqrt(3))
#' 
#' @export silencio
silencio <- function(...) suppressWarnings(suppressMessages(...))
