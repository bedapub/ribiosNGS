#' Calculate normalisation factor if not
#' @param dgeList A \code{DGEList} object
#' Calculate the normalisation factors if not done yet
#' @importFrom edgeR calcNormFactors
#' @return Updated \code{dgeList} object with \code{norm.factors} filled
#' @export
calcNormFactorsIfNot <- function(dgeList) {
  if(all(dgeList$samples$norm.factors==1))
    dgeList <- edgeR::calcNormFactors(dgeList)
  return(dgeList)
}

#' cpm for EdgeObject
#' @param y An EdgeObject object
#' @param ... Passed to \code{cpm}
#' @seealso \code{\link[edgeR]{cpm}}
#' @export
cpm.EdgeObject <- function(y, ...) {
	edgeR::cpm(dgeList(y),...)
}

