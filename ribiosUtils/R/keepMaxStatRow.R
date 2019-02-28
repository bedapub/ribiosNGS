#' @export isMaxStatRow
isMaxStatRow <- function (matrix,
                          keys,
                          keepNArows = TRUE,
                          stat = function(x) mean(x, na.rm = TRUE),
                          ...) {
  if (!is.matrix(matrix)) 
    matrix <- as.matrix(matrix)
  if (!is.character(keys)) 
    keys <- as.character(keys)
  haltifnot(!missing(keys) && identical(length(keys), nrow(matrix)), 
            msg = "'keys' must be of the same length as the row number of the matrix")
  if (is.character(stat)) 
    stat <- get(stat, envir = parent.frame())
  haltifnot(is.function(stat), msg = "'stat' must be either a function for some certain statistic, e.g. sd, or the name of such a function\n")
  rows.has.index <- !is.na(keys) & keys != ""
  matrix.indexed <- matrix[rows.has.index, , drop = FALSE]
  matrix.indexed.rowStat <- apply(matrix.indexed, 1L, stat,...)
  if (any(is.na(matrix.indexed.rowStat))) 
    warning("Statistics of some rows are NA and they will be discarded. You may want to double check this.\n")
  matrix.indexed.fac <- factor(keys[rows.has.index])
  rows.by.index <- split(1:nrow(matrix.indexed), matrix.indexed.fac)
  stat.by.index <- split(matrix.indexed.rowStat, matrix.indexed.fac)
  max.rows <- sapply(1:nlevels(matrix.indexed.fac), function(x) rows.by.index[[x]][which.max(stat.by.index[[x]])])
  if (is.list(max.rows)) 
    max.rows <- unlist(max.rows)
  matrix.remain <- rep(FALSE, nrow(matrix))
  if (keepNArows) 
    matrix.remain[!rows.has.index] <- TRUE
  matrix.remain[rows.has.index][max.rows] <- TRUE
  return(matrix.remain)
}

#' @export keepMaxStatRowInd
keepMaxStatRowInd <- function(matrix,
                              keys,
                              keepNArows = TRUE,
                              stat = function(x) mean(x, na.rm = TRUE),
                              ...) {
  which(isMaxStatRow(matrix=matrix,
                     keys=keys,
                     keepNArows=keepNArows,
                     stat=stat, ...))
}

#' KEEP ROWS WITH THE MAXIMUM STATISTIC
#' 
#' A common task in expression analysis is to collapse multiple features that
#' are mapped to the same gene by some statistic. This function does this job
#' by keeping the matrix row (normally features) with the higheest statistic
#' specified by the user.
#' 
#' \code{isMaxStatRow} returns a logical vector, with rows with maximal
#' statistics each key as \code{TRUE} and otherwise as \code{FALSE}.
#' \code{keepMaxStatRowInd} returns the integer indices of such rows. Finally
#' \code{keepMaxStatRow} returns the resulting matrices.
#' 
#' For use see examples
#' 
#' @aliases keepMaxStatRow isMaxStatRow keepMaxStatRowInd
#' @param matrix A numeric matrix
#' @param keys A vector of character giving the keys the rows are mapped to. A
#' common scenario is that each row represents one probeset, while the vector
#' keys give the genes that the probesets are mapped to. Thus keys can be
#' redundant, namely multiple probesets can map to the same gene.
#' @param keepNArows Logical, whether rows with \code{NA} as their keys should
#' be kept (\code{TRUE}) or should be discarded (\code{FALSE})
#' @param stat The function to calculate the univariate statistic. By default
#' the \code{NA}-robust mean is used.
#' @param levels How should the information of the levels of keys, e.g. unique
#' keys, be kept. \code{dicard} will discard this information, \code{rownames}
#' will make the unique keys (potentially with \code{NA}s) as row names of the
#' output matrix, and \code{attribute} will append an attribute named
#' \code{levels} to the output matrix.
#' @param \dots Other parameters passed to the \code{stat} function
#' @return A numeric matrix with rows mapped to unique keys, selected by the
#' maximum statistics. See examples below
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' myFun1 <- function(x) mean(x, na.rm=TRUE)
#' myFun2 <- function(x) sd(x, na.rm=TRUE)
#' mat1 <- matrix(c(1,3,4,-5,
#'                  0,1,2,3,
#'                  7,9,5,3,
#'                  0,1,4,3), ncol=4, byrow=TRUE)
#' keys1 <- c("A", "B", "A", "B")
#' 
#' isMaxStatRow(mat1, keys1, stat=myFun1)
#' isMaxStatRow(mat1, keys1, stat=myFun2)
#' 
#' keepMaxStatRowInd(mat1, keys1, stat=myFun1)
#' keepMaxStatRowInd(mat1, keys1, stat=myFun2)
#' 
#' keepMaxStatRow(mat1, keys1, stat=myFun1)
#' keepMaxStatRow(mat1, keys1, stat="myFun2")
#' keepMaxStatRow(mat1, keys1, stat="myFun2", levels="discard")
#' keepMaxStatRow(mat1, keys1, stat="myFun2", levels="attribute")
#' 
#' mat2 <- matrix(c(1,3,4,5,
#'                  0,1,2,3,
#'                  7,9,5,3,
#'                  0,1,4,3,
#'                  4,0,-1,3.1,
#'                  9,4,-3,2,
#'                  8,9,1,2,
#'                  0.1,0.2,0.5,NA,
#'                  NA, 4, 3,NA), ncol=4, byrow=TRUE,
#'                dimnames=list(LETTERS[1:9], NULL))
#' keys2 <- c("A", "B", "A", "B", NA, NA, "C", "A", "D")
#' 
#' isMaxStatRow(mat2, keys2, keepNArows=FALSE, stat=myFun1)
#' keepMaxStatRowInd(mat2, keys2, keepNArows=FALSE, stat=myFun1)
#' 
#' keepMaxStatRow(mat2, keys2, keepNArows=FALSE, stat=myFun1)
#' keepMaxStatRow(mat2, keys2, keepNArows=TRUE, stat=myFun1)
#' keepMaxStatRow(mat2, keys2, keepNArows=TRUE, stat=myFun1, levels="discard")
#' keepMaxStatRow(mat2, keys2, keepNArows=TRUE, stat=myFun1, levels="attribute")
#' 
#' \dontrun{
#' ## don't run
#' mat3 <- matrix(c(1,2,3,4), nrow=1L)
#' keys3 <- "A"
#' keepMaxStatRow(mat3, keys3, keepNArows=FALSE)
#' 
#' ## don't run
#' mat4 <- matrix(c(1,2,3,4,
#'                  NA,NA,NA,NA), nrow=2L, byrow=TRUE)
#' keys4 <- c("A", "B")
#' keepMaxStatRow(mat4, keys4, keepNArows=FALSE)
#' keepMaxStatRow(mat4, keys4, keepNArows=TRUE)
#' }
#' 
#' @export keepMaxStatRow
keepMaxStatRow <- function(matrix, keys, keepNArows=TRUE,
                           stat=function(x) mean(x, na.rm=TRUE),
                           levels=c("rownames", "attribute", "discard"),
                           ...) {
  levels <- match.arg(levels)
  matrix.remain <- isMaxStatRow(matrix=matrix, keys=keys,
                                keepNArows=keepNArows,
                                stat=stat, ...)
  res <- matrix[matrix.remain,,drop=FALSE]
  if(levels!="discard") {
    newnames <- keys[matrix.remain]
    if(levels=="rownames") {
      rownames(res) <- newnames
    } else if (levels=="attribute") {
      attr(res, "levels") <- newnames
    }
  }
  return(res)
}
