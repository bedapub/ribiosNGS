setClass("fcol", representation(base="character"), contains="character")

#' A simple data structure to hold PCA scores in numeric matrix and explained variance in dedicated slots
setClass("PCAScoreMatrix", representation(expvar="numeric"), contains="matrix")

#' Construct a PCAScoreMatrix object
#' @param scoreMatrix Numeric matrix, objects in rows and PCs in columns
#' @param expvar Numeric vector, length must equal the number of columns of \code{scoreMatrix}, explained variance by respective PCs
#' 
#' This function is usually not called by the end user; instead, it is used by the function \code{\link{pcaScores}}
#' @examples 
#' PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
PCAScoreMatrix <- function(scoreMatrix, expvar) {
  stopifnot(ncol(scoreMatrix)==length(expvar))
  res <- new("PCAScoreMatrix", scoreMatrix)
  res@expvar <- expvar
  return(res)
}

#' Show PCAScoreMatrix object
#' @param object A PCAScoreMatrix object
setMethod("show", "PCAScoreMatrix",function(object) {
  expvar <- object@expvar
  cat(sprintf("PCAScoreMatrix with %d dimensions\n", length(object@expvar)))
  show(object@.Data)
  cat("Explained variances:", paste(ribiosUtils::percentage(object@expvar), 
                                    collapse=","),
      sprintf("(%s in total)", ribiosUtils::percentage(sum(object@expvar))))
})
