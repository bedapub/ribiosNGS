#' Transform p-values to a continuous score
#'
#' The function maps p value between 0 and 1 to a continuous score
#' by the following equation: abs(log10(p))*sign.
#' 
#' @param p \emph{p}-value between (0,1]
#' @param sign Sign of the score, either positive (in case of a positive number), 
#'             negative (in case of a negative number), or zero.
#' @examples
#' testPvals <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1)
#' pScore(testPvals)
#' testPvalSign <- rep(c(-1,1), 3)
#' pScore(testPvals, sign=testPvalSign)
pScore <- function(p, sign=1) {
  abs(log10(p)) * sign(sign)
}
