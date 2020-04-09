#' @include AllClasses.R
NULL

#' Return the size of the smallest group
#' 
#' @param obj A \code{DGEList} or \code{EdgeObject} object
#' @return Integer
#' 
#' @examples 
#' y <- matrix(rnbinom(12000,mu=10,size=2),ncol=6)
#' d <- DGEList(counts=y, group=rep(1:3,each=2))
#' minGroupCount(d) ## 2 
#' d2 <- DGEList(counts=y, group=rep(1:2,each=3))
#' minGroupCount(d2) ## 3
#' d3 <- DGEList(counts=y, group=rep(1:3, 1:3))
#' minGroupCount(d3) ## 1
#' 
#' @export
minGroupCount <- function(obj) {
  UseMethod("minGroupCount")
}

#' @describeIn minGroupCount Return the size of the smallest group defined in
#'  the \code{DGEList} object
#' @export
minGroupCount.DGEList <- function(obj) {
  groups <- obj$samples$group
  if(!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(min(table(groups)))
}
#' @describeIn minGroupCount Return the size of the smallest group defined in
#'   the \code{EdgeObject} object
#' @export
minGroupCount.EdgeObject <- function(obj) {
  groups <- groups(obj@designContrast)
  if(!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(min(table(groups)))
}
