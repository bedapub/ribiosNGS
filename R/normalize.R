#' @include AllClasses.R

#' @describeIn normalize Normalize an EdgeObject by calculating normalization
#'   factors using the given method
#' @param obj An \code{EdgeObject} object
#' @param method Method passed to \code{\link[edgeR]{calcNormFactors}}.
#' @importMethodsFrom BiocGenerics normalize
#' @export
setMethod("normalize", "EdgeObject", function(object, method="RLE", ...) {
  object@dgeList <- calcNormFactors(object@dgeList, method=method, ...)
  return(object)
})
