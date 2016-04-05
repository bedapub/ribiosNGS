#' Outlier detection with Nalimov test
#'
#' #' @param x Numeric vector
#' @param sig Significance level
#' @return Numeric vector with outlier(s) removed
#' @details The function performs the Nalimov test in a non-recursive fashion
#' @author Jitao David Zhang and Clemens Brogers
#' @examples
#' testVec <- c(30.41, 30.05, 30.49, 29.22, 30.40, 30.42)
#' nalimov(testVec, sig="0.95")
#' @export
#' @useDynLib ribiosDemo bios_nalimov
nalimov <- function(x, sig=c("0.95", "0.99", "0.995")) {
  sig <- match.arg(sig)
  if(sig=="0.95") {
    isig <- 0L
  } else if (sig=="0.99") {
    isig <- 1L
  } else {
    isig <- 2L
  }
  .Call("bios_nalimov", as.numeric(x), isig)
}
