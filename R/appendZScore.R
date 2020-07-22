#' @include oseudoT.R
NULL

## in pseudoT.R, we implement the pseudoT statistic.
## We can also use zscore, which do not allow comparison between studies of different sample sizes, but deliver less extreme values.

#' Append zscores to dgeTable
#' @param edgeResult An \code{EdgeResult} object
#' @param dgeTable A \code{data.frame}, derived from \code{dgeTables} or \code{dgeTable} usually.
#' @return A new \code{data.frame} with one new column, \code{zScore}, containing the z-score transformed from p-values. If that column exists, it will be rewritten.
#' The function is similar to \code{appendPseudoT}, with the difference that the Gaussian distribution is used instead of the t-distribution. This solution
#' delivers less extreme values, because t-distribution is heavy-tailed. It allows comparison between studies of different sample sizes.
#' @importFrom ribiosUtils pQnormScore
#' @seealso \code{\link{appendPseudoT}}
#' @export
appendZScore <- function(dgeTable) {
  res <- dgeTable %>%
    mutate(zScore=ribiosUtils::pQnormScore(PValue, logFC))
  return(res)
}

#' Append dgeTables with z-scores
#' @param edgeResult An \code{EdgeResult} object.
#' @return Similar as \code{dgeTables}, a list of \code{data.frame}, but with an additional column \code{zScore}.
#' @seealso \code{\link{appendZScore}}, \code{\link{dgeTableWithZScore}}, \code{\link{dgeTableWithPseudoT}}
#' @export
dgeTablesWithZScore <- function(edgeResult) {
  dts <- dgeTables(edgeResult)
  res <- lapply(dts, function(x) appendZScore(edgeResult, x))
  return(res)
}

#' Append dgeTable with z-scores
#' @param edgeResult An \code{EdgeResult} object.
#' @param contrast A character string, or integer index, or \code{NULL}, to specify the contrast. If \code{NULL}, results of all contrasts are returned.
#' @return Similar as \code{dgeTable}, a \code{data.frame}, with an additional column \code{zScore}.
#' @seealso \code{\link{dgeTablesWithZScore}}, \code{\link{dgeTable}}, \code{\link{dgeTableWithPseudoT}}
#' @export
dgeTableWithZScore <- function(edgeResult, contrast=NULL) {
  tbl <- appendZScore(dgeTable(edgeResult, contrast=contrast))
  return(tbl)
}

