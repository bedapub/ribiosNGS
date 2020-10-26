#' @include AllClasses.R AllGenerics.R AllMethods.R
NULL

utils::globalVariables(c("PValue", "logFC"))

#' Convert p-values to t-statistics
#' @param p Numeric, a numeric vector between 0 and 1.
#' @param df Numeric, degree of freedom.
#' @param sign Logical or integer, positive numbers or \code{TRUE} are interpreted as positive, and negative numbers or \code{TRUE} are interpreted as negative.
#' @param replaceZero Logical, whether small p values or 0 should be replaced by a sufficient small number. Default and recommended: \code{TRUE}
#' @importFrom stats qt
#' @importFrom ribiosUtils replaceZeroPvalue
#' @examples 
#' pVals <- 10^(seq(-11,0))
#' signs <- rep(c(TRUE, FALSE), 6)
#' tVals <- pseudoTfromPvalue(pVals, 5, sign=signs)
#' logFCs <- rep(c(1.2,-1.2),6)
#' tValsLogFCs <- pseudoTfromPvalue(pVals, 5, sign=logFCs)
#' @export
pseudoTfromPvalue <- function(p, df, sign, replaceZero=TRUE) {
  if(replaceZero) {
    p <- replaceZeroPvalue(p, factor=2)
  }
  if(is.logical(sign))
    sign <- ifelse(sign, 1, -1)
  ts <- abs(stats::qt(p=p/2, df=df)) * sign(sign)
  return(ts)
}

#' Append degree of freedom and pseudo t-statistics to dgeTable
#' @param edgeResult An \code{EdgeResult} object
#' @param dgeTable A \code{data.frame}, derived from \code{dgeTables} or \code{dgeTable} usually.
#' @return A new \code{data.frame} with two new columns, \code{df} and \code{pseudoT}, containing degree of freem and the (pseudo) t-statistic, respective.y
#' The function relies on the fact that the degree of freedom (`df.residual` in GLM result) is the same for all genes.
#' If this is not the case, it will use the `FeatureName` column in gene annotation to match the degree of freedoms.
#' The function is only used internally by \code{dgeTablesWithPseudoT} and \code{dgeTableWithPseudoT}.
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate
#' @note the \code{lower.tail} option in \code{qt} is not vectorized, therefore the sign should be provided separately.
appendPseudoT <- function(edgeResult, dgeTable) {
  df <- unique(edgeResult@dgeGLM$df.residual)
  if(length(df)==1) {
    dgeTable <- dgeTable %>% mutate(df=df)
  } else {
    dfTbl <- data.frame(FeatureName=edgeResult@dgeGLM$genes$FeatureName,
                        df=edgeResult@dgeGLM$df.residual)
    dgeTable <- dgeTable %>% dplyr::left_join(dfTbl, by="FeatureName")
  }
  res <- dgeTable%>%
    mutate(pseudoT=pseudoTfromPvalue(PValue, df, logFC))
  return(res)
}

#' Append dgeTables with pseudo t-statistic
#' @param edgeResult An \code{EdgeResult} object.
#' @return Similar as \code{dgeTables}, a list of \code{data.frame}, but with additional columns \code{df} (degree of freedom) and \code{pseudoT}.
#' The pseudo t-statistic is calculated based on the P-value of the likelihood ratio test and the residual degree of freedom by the function. \code{\link{qt}}, and its sign is given by the sign of logFC.
#' @seealso \code{\link{dgeTableWithPseudoT}}
#' @export
dgeTablesWithPseudoT <- function(edgeResult) {
  dts <- dgeTables(edgeResult)
  res <- lapply(dts, function(x) appendPseudoT(edgeResult, x))
  return(res)
}

#' Append dgeTable with pseudo t-statistic
#' @param edgeResult An \code{EdgeResult} object.
#' @param contrast A character string, or integer index, or \code{NULL}, to specify the contrast. If \code{NULL}, results of all contrasts are returned.
#' @return Similar as \code{dgeTable}, a \code{data.frame}, but with additional columns \code{df} (degree of freedom) and \code{pseudoT}.
#' The pseudo t-statistic is calculated based on the P-value of the likelihood ratio test and the residual degree of freedom by the function \code{\link{qt}}, and its sign is given by the sign of logFC.
#' @seealso \code{\link{dgeTablesWithPseudoT}}, \code{\link{dgeTable}}
#' @export
dgeTableWithPseudoT <- function(edgeResult, contrast=NULL) {
  tbl <- appendPseudoT(edgeResult, dgeTable(edgeResult, contrast=contrast))
  return(tbl)
}
