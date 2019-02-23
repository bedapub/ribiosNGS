##------------##
## S3: PCAScoreMatrix
##------------##

#' Construct a S3-class PCAScoreMatrix object
#' 
#' @param scoreMatrix Numeric matrix, objects in rows and PCs in columns
#' @param expVar Numeric vector, length must equal the number of columns of \code{scoreMatrix}, explained variance by respective PCs
#' 
#' @return A S3-class \code{PCAScoreMatrix} object with two items: scoreMatrix s and expVar (explained variance).
#' 
#' @seealso \code{as.matrix.PCAScoreMatrix}, \code{expVar.PCAScoreMatrix}, \code{print.PCAScoreMatrix}.
#' This function is usually not called by the end user; instead, it is used by the function \code{\link{pcaScores}}
#' 
#' @examples 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
PCAScoreMatrix <- function(scoreMatrix, expVar) {
  stopifnot(ncol(scoreMatrix)==length(expVar))
  res <- list(scoreMatrix=scoreMatrix, expVar=expVar)
  class(res) <- "PCAScoreMatrix"
  return(res)
}

#' Print PCAScoreMatrix
#' @param x A \code{PCAScoreMatrix} S3-object
#' @param ... Ignored
#' 
#' @return NULL, side effect is used
#' 
#' @examples 
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' myPCmat
print.PCAScoreMatrix <- function(x, ...) {
  expVar <- x$expVar
  cat(sprintf("PCAScoreMatrix with %d dimensions\n", length( expVar)))
  show(x$scoreMatrix)
  cat("Explained variances:", paste(ribiosUtils::percentage( expVar), 
                                    collapse=","),
      sprintf("(%s in total)\n", ribiosUtils::percentage(sum( expVar))))
  cat("Options\n")
  cat("-- Use 'as.matrix' to turn this object into a score matrix\n")
  cat("-- Use 'expVar' to extract explained variances\n")
  cat("-- Use 'expVarLabel' to generate labels of explained variances")
}

#' Coerece a PCAScoreMatrix into score matrix
#' 
#' @param x A \code{PCAScoreMatrix} S3 object
#' @param ... Currently ignored
#' 
#' @return A numeric matrix, the score matrix
#' 
#' @examples
#' myPCmat <- PCAScoreMatrix(matrix(rnorm(15),ncol=3), c(0.25, 0.15, 0.1))
#' as.matrix(myPCmat)
as.matrix.PCAScoreMatrix <- function(x, ...) {
  return(x$scoreMatrix)
}

##---------##
## generics
##---------##
setGeneric("fcol", function(object, base) standardGeneric("fcol"))
setGeneric("fcbase", function(object) standardGeneric("fcbase"))

##---------##
## methods
##---------##
## methods
setMethod("fcol", c("character", "character"), function(object, base) {
  new("fcol", object, base=base)
})
setMethod("fcbase", "fcol", function(object) return(object@base))

setMethod("show", "fcol", function(object) {
  acol <- as.character(object)
  bcol <- fcbase(object)
  cat("Factor-matching colors\n",
      "Colors: (", length(acol), "):", ribiosUtils::chosenFew(acol),"\n",
      "Base colors (", length(bcol), "):", ribiosUtils::chosenFew(fcbase(object)), "\n",
      sep="")
})

EXPVAR_COMPACT_FMTSTR <-"PC%d (%s)"
EXPVAR_FULL_FMTSTR <- "Principal component %d (%s variance explained)"

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
  if(missing(choices) || is.null(choices) || is.na(choices))
    choices <- seq(along=x$expVar)
  res <- x$expVar[choices]
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
expVarLabel.PCAScoreMatrix <- function(x, choices, compact=FALSE) {
  nc <- ncol(as.matrix(x))
  pc <- 1:nc
  if(missing(choices) || is.null(choices)) {
    choices <- 1:nc
  } 
  expVarChoices <- expVar(x)[choices]
  pcChoices <- pc[choices]
  
  fmt <- ifelse(compact, EXPVAR_COMPACT_FMTSTR, EXPVAR_FULL_FMTSTR)
  
  res <- sprintf(fmt,
                 pcChoices,
                 ribiosUtils::percentage(expVarChoices))
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
  expvs <- expVar(x)
  pc <- seq(along=expvs)
  if(missing(choices) || is.null(choices) || is.na(choices)) {
    choices <- seq(along=expvs)
  } 
  expVarChoices <- expvs[choices]
  pcChoices <- pc[choices]
  
  fmt <- ifelse(compact, EXPVAR_COMPACT_FMTSTR, EXPVAR_FULL_FMTSTR)
  
  res <- sprintf(fmt,
                 pcChoices,
                 ribiosUtils::percentage(expVarChoices))
  return(res)
}

