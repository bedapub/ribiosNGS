#' Encode factors into plain character strings
#'
#' @param fac A factor vector
#'
#' \code{encodeFactor} encodes a factor vector into a string vector that can be
#' decoded by \code{decodeFactor}. This is especially useful when factors need
#' to be written into relational database management systems (RDMS) while it is
#' desired that the order of the levels is kept.
#'
#' @examples
#' testFac <- factor(c("HBV", "FCB", "BVB", "HBV"), levels=c("HBV", "FCB", "BVB"))
#' testFacStr <- encodeFactor(testFac)
#' testFacOut <- decodeFactor(testFacStr)
#' testthat::expect_equivalent(testFac, testFacOut)
#'
#' @export
encodeFactor <- function(fac) {
  prefix <- "_factor%d."
  newLevels <- paste(sprintf(prefix, 1:nlevels(fac)),
                     levels(fac), sep="")
  levels(fac) <- newLevels
  res <- as.character(fac)
  return(res)
}

#' Decode factors from plain character strings
#'
#' @param str A string vector
#'
#' \code{decodeFactor} decodes a factor vector from a string vector that was
#' encoded by \code{encodeFactor}. This is especially useful when factors need
#' to be written into relational database management systems (RDMS) while it is
#' desired that the order of the levels is kept.
#'
#' @examples
#' testFac <- factor(c("HBV", "FCB", "BVB", "HBV"), levels=c("HBV", "FCB", "BVB"))
#' testFacStr <- encodeFactor(testFac)
#' testFacOut <- decodeFactor(testFacStr)
#' testthat::expect_equivalent(testFac, testFacOut)
#'
#' @export
decodeFactor <- function(str) {
  fac <- gsub("^_factor([0-9]*)\\.(.*)$", "\\2",str)
  levelInt <- as.integer(gsub("^_factor([0-9]*)\\.(.*)$", "\\1",str))
  decodedLevels <- unique(fac[order(levelInt, decreasing=FALSE)])
  res <- factor(fac, levels=decodedLevels)
  return(res)
}



