#' Rename contrast by a pair of vectors
#' @param edgeResult An \code{EdgeResult} object
#' @param oldContrastName A vector of character strings giving old contrast names
#' @param newContrastName completeA vector of character strings giving new contrast names, which match the \code{oldContrastName} one to one.
#' @return A new \code{EdgeResult} object
#' @export
renameContrast <- function(edgeResult, oldContrastName, newContrastName) {
  stopifnot(all(oldContrastName %in% contrastNames(edgeResult)))
  stopifnot(length(oldContrastName) == length(newContrastName))
  for(i in seq(along=oldContrastName)) {
    oldContrast <- oldContrastName[i]
    newContrast <- newContrastName[i]
    isMatching <- colnames(edgeResult@designContrast@contrasts)==oldContrast
    colnames(edgeResult@designContrast@contrasts)[isMatching] <- newContrast
    isMatching <- names(edgeResult@dgeTables)==oldContrast
    names(edgeResult@dgeTables)[isMatching] <- newContrast
  }
  return(edgeResult)
}
#' Rename contrast by a function
#' @param edgeResult An \code{EdgeResult} object
#' @param func A function receiving a vector of character strings as input, and returns
#'   another vector of the same length as output, for instance \code{gsub}.
#' The function can be called to rename contrasts
#' @return A new \code{EdgeResult} object
#' @export
renameContrastByFunc <- function(edgeResult, func) {
  colnames(edgeResult@designContrast@contrasts) <- 
    do.call(func, list(colnames(edgeResult@designContrast@contrasts)))
  names(edgeResult@dgeTables) <- 
    do.call(func, list(names(edgeResult@dgeTables)))
  return(edgeResult)
}
