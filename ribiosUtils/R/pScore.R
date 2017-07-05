#' Transform p-values to a continuous score
#'
#' The function maps p value between 0 and 1 to a continuous score
#' by the following equation: abs(log10(p))*sign.
#' 
#' @param p \emph{p}-value between (0,1]
#' @param sign Sign of the score, either positive (in case of positive numbers), 
#'             negative (in case of negative numbers), or zero. 
#'             In case a logical vector, \code{TRUE} is interpreted as positive
#'             and \code{FALSE} is interpreted as negative.
#'             
#' @examples
#' testPvals <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1)
#' pScore(testPvals)
#' testPvalSign <- rep(c(-1,1), 3)
#' pScore(testPvals, sign=testPvalSign)
#' testLog <- rep(c(TRUE, FALSE),3)
#' pScpre(testPvals, testLog)
pScore <- function(p, sign=1) {
  if(is.logical(sign))
    sign <- ifelse(sign, 1, -1)
  abs(log10(p)) * sign(sign)
}
