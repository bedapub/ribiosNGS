print.FishersMethodResult <- function(x) {
  cat("Combined P by Fisher's Method:\n")
  cat(sprintf("p=%g\n", x$p))
  cat(sprintf("Chisq=%g, df=%d\n", x$chisq, x$df))
  cat("-- Use 'x$p' to extract the p-value\n")
  cat("-- Use 'x$chisq' and 'x$df' to extract Chi-square statistic and degree of freedom, respectively\n")
  if(!is.null(x$validp))
    cat("-- Valid p-values used for calculation are available. Use 'x$validp' to extract them.")
}

#' Fisher's method to combine multiple p-values
#' @param p Numeric vector, p values to be combined
#' @param returnValidP Logical, whether the valid p-values used should be returned as part of the list
#' @return A \code{FisherMethodResult} S3 object, a list of following elements
#' \enumerate{
#' \item chisq: Chi-square statistic
#' \item df: Degree of freedom (which is twice the count of the valid p-values used for calculation)
#' \item p: p-value
#' \item validp (optional): valid p-values used for the calculation
#' }
#' The function returns the combined p-value using the sum of logs (Fisher's) method
#' @note The function was adapted from metap::sumlog
#' 
#' @examples
#' ps <- c(0.05, 0.75)
#' fishersMethod(ps)
#' fishersMethod(ps, returnValidp=TRUE)
fishersMethod <- function(p, returnValidp=FALSE) {
  keep <- !is.na(p) & p>0 & p<=1
  if(!all(keep)) {
    warning("P-values outside (0,1] omitted")
  }
  lnp <- log(p[keep])
  chisq <- (-2) * sum(lnp)
  df <- 2 * length(lnp)
  if (sum(keep) == 1) {
    warning("Only one p-value provided. The original p value is returned.")
    res <- list(chisq=chisq, df=df,
                p=p[keep])
  } else if (!any(keep)) {
    res <- list(chisq=NA, df=NA, p=NA)  
  } else {
    res <- list(chisq = chisq, df = df, 
                p = pchisq(chisq, df, lower.tail = FALSE))
  }
  if(returnValidp)
    res$validp <- p[keep]
  class(res) <- "FishersMethodResult"
  return(res)
}