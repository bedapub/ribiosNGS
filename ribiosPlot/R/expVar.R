##------------##
## expVar
##------------##
#' S3 function expVar to extract explained variance from prcomp and PCAScoreMatrix objects
#' 
#' @param x A \code{prcomp} or \code{PCAScoreMatrix} object.
#' @param choices Either missing, or an integer vector of indices, indicating which PCs should be returned.
#' 
#' @return A numeric vector of variance explained
#' 
#' @examples 
#' myMat <- matrix(rnorm(100), ncol=10)
#' myPrcomp <- prcomp(myMat)
#' myPcaScoreMatrix <- pcaScores(myPrcomp, choices=NULL) 
#' expVar(myPrcomp)
#' expVar(myPcaScoreMatrix)
#'
#' expVar(myPrcomp, 1:5)
#' expVar(myPcaScoreMatrix, 1:5)
expVar <- function(x, choices) UseMethod("expVar")

#'@describeIn expVar Extract explained variance from a prcomp object
expVar.prcomp <- function(x, choices) {
  vars <- x$sdev^2
  if(missing(choices) || is.null(choices) || is.na(choices))
    choices <- seq(along=vars)
  res <- vars[choices]/sum(vars)
  return(res)
}

#'@describeIn expVar Extract explained variance from a PCAScoreMatrix object
expVar.PCAScoreMatrix <- function(x, choices) {
  ev <- attr(x, "expVar")
  if(missing(choices) || is.null(choices) || is.na(choices))
    choices <- seq(along=ev)
  res <- ev[choices]
  return(res)
}

##------------##
## expVarLabel
##------------##
#' Generic function expVarLabel to generate a label of explained variance from prcomp and PCAScoreMatrix objects
#' @param x \code{prcomp} or \code{PCAScoreMatrix} Object
#' @param choices Integer indices of which PCs to be returned
#' @param compact Logical, whether a compact format is returned, see example
expVarLabel <- function(x, choices, compact) UseMethod("expVarLabel")

#' Helper function to print PC and explained variances
#' @param ev A numeric vector of explained variances
#' @param choices An integer vector to indicate which PCs to be returned. If \code{NULL} or \code{NA} or missing, all elements are returned.
#' @param compact Logical, either a \code{compact} label is returned, see examples.
getExpVarLabel <- function(ev, choices, compact=FALSE) {
  if(missing(choices) || is.null(choices) || is.na(choices))
    choices <- seq(along=ev)
  
  fmt <- ifelse(compact, "PC%d (%s)",  "Principal component %d (%s variance explained)")
  
  res <- sprintf(fmt,
                 choices,
                 ribiosUtils::percentage(ev))
  return(res)
}

#' Labels of principal components from PCAScoreMatrix
#' 
#' @param x A \code{PCAScoreMatrix} object
#' @param choices Either a logical/integer vector to indicate which PCs to be returned, 
#'   or \code{NULL} or missing, in which case all PCs are returned
#' @param compact Logical, either a \code{compact} label is returned, see examples.
#' 
#' @return A character string vector of the same length as \code{choices} (or the same length as the column count of the PCAScoreMatrix), 
#'   which are the labels of the PCs
#' 
#' @examples 
#' pcaMat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' expVarLabel(pcaMat)
#' expVarLabel(pcaMat, choices=1:2)
#' expVarLabel(pcaMat, choices=1:2, compact=TRUE)
#' expVarLabel(pcaMat, choices=c(1,3), compact=TRUE)
expVarLabel.PCAScoreMatrix <- function(x, choices, compact=FALSE) {
  ev <- expVar(x, choices)
  
  res <- getExpVarLabel(ev=ev, choices=choices, compact=compact)
  return(res)
}

#' Labels of principal components from prcomp
#' 
#' @param x A \code{PCAScoreMatrix} object
#' @param choices Either a logical/integer vector to indicate which PCs to be returned, 
#'   or \code{NULL} or missing, in which case all PCs are returned
#' @param compact Logical, either a \code{compact} label is returned, see examples.
#' 
#' @return A character string vector of the same length as \code{choices} (or the same length as the column count of the scores), 
#'   which are the labels of the PCs
#' 
#' @examples 
#' myPr <- prcomp(matrix(rnorm(100), ncol=5))
#' expVarLabel(myPr)
#' expVarLabel(myPr, choices=1:2)
#' expVarLabel(myPr, choices=1:2, compact=TRUE)
expVarLabel.prcomp <- function(x, choices, 
                               compact=FALSE) {
  ev <- expVar(x, choices)
  res <- getExpVarLabel(ev=ev, choices=choices, compact=compact)
  return(res)
}
