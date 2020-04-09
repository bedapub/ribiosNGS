#' @include AllClasses.R
NULL

#' Normalize an EdgeObject
#'
#' @describeIn normalize Normalize an EdgeObject by calculating normalization
#'   factors using the given method
#' @param object An \code{EdgeObject} object.
#' @param method Method passed to \code{\link[edgeR]{calcNormFactors}}.
#' @param ... Other parameters passed to \code{\link[edgeR]{calcNormFactors}}.
#' @importMethodsFrom BiocGenerics normalize
#' @export
setMethod("normalize", "EdgeObject", function(object, method="RLE", ...) {
  object@dgeList <- calcNormFactors(object@dgeList, method=method, ...)
  return(object)
})
