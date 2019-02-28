#' Transform a matrix into a long-format data.frame
#' 
#' The function converts a matrix into a long-format, three-column data.frame,
#' containing row, columna nd value. Such \sQuote{long} data.frames can be
#' useful in data visualization and modelling.
#' 
#' The function converts a matrix into a three-column, \sQuote{long} format
#' data.frame containing row names, column names, and values of the matrix.
#' 
#' @param mat A matrix
#' @param row.names Character, row names to appear in the \code{data.frame}. If
#' missing, the \code{rownames} of the matrix will be used. If set to
#' \code{NULL}, or if the matrix \code{rownames} are \code{NULL}, a integer
#' index vector starting from 1 will be used.
#' @param col.names Charater, column names to appear in the \code{data.frame}.
#' The rule of handling missing or NULL parameters is the same as
#' \code{row.names} described above.
#' @param longdf.colnames Character, column names of the output long data frame
#' @return A \code{data.frame} object with three columns: \code{row},
#' \code{column} and \code{value}. If the input matrix is of dimesion
#' \code{MxN}, the returning \code{data.frame} is of the dimension \code{MNx3}.
#' @note The length of \code{row.names} and \code{col.names} should be as the
#' same as the matrix dimension. Otherwise the function raises warnings.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' test.mat <- matrix(1:12, ncol=4, nrow=3, dimnames=list(LETTERS[1:3],
#' LETTERS[1:4]))
#' print(test.mat)
#' print(matrix2longdf(test.mat))
#' print(matrix2longdf(test.mat, longdf.colnames=c("From", "To", "Time")))
#' 
#' @export matrix2longdf
matrix2longdf <- function(mat,
                          row.names, col.names,
                          longdf.colnames=c("row","column","value")) {
  if(missing(row.names)) row.names <- rownames(mat)
  if(missing(col.names)) col.names <- colnames(mat)
  
  if(is.null(row.names)) row.names <- 1:nrow(mat)
  if(is.null(col.names)) col.names <- 1:ncol(mat)
  
  value <- as.vector(mat)
  if(length(row.names)!=nrow(mat))
    warning("row.names is inconsistent with the matrix dim")
  if(length(col.names)!=ncol(mat))
    warning("col.names is inconsistent with the matrix dim")
  
  rn <- rep(row.names, ncol(mat))
  cn <- rep(col.names, each=nrow(mat))
  res <- data.frame(row=rn,
                    column=cn,
                    value=value)
  colnames(res) <- longdf.colnames
  return(res)
}

#' Convert a long-format data frame into matrix
#' 
#' Input data.frame must contain at least three columns: one contains row names
#' (specified by \code{row.col}), one contains column names
#' (\code{column.col}), and one contains values in matrix cells
#' (\code{value.col}). The output is a 2D matrix.
#' 
#' @param df Long-format data frame
#' @param row.col Character or integer, which column of the input data.frame
#' contains row names?
#' @param column.col Character or integer, which column contains column names?
#' @param value.col Character or integer, which column contains matrix values?
#' @return A 2D matrix equivalent to the long-format data frame
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{matrix2longdf}
#' @examples
#' 
#' test.df <- data.frame(H=c("HSV", "BVB", "HSV", "BVB"),
#' A=c("FCB", "S04", "S04", "FCB"),
#' score=c(3, 1, 1, 0))
#' longdf2matrix(test.df, row.col=1L, column.col=2L, value.col=3L)
#' 
#' data(Indometh)
#' longdf2matrix(Indometh, row.col="time", column.col="Subject",value.col="conc")
#' longdf2matrix(Indometh, row.col="Subject", column.col="time", value.col="conc")
#' 
#' @export longdf2matrix
longdf2matrix <- function(df, row.col = 1L, column.col = 2L, value.col = 3L,
                          missingValue=NULL) {
    warnMissingValue <- is.null(missingValue)
    if(warnMissingValue)
      missingValue <- NA
    sub <- as.data.frame(df[, c(row.col, column.col, value.col)])
    subrows <- unique(sub[,1L])
    subcols <- unique(sub[,2L])
    r.ind <- match(sub[,1L], subrows)
    c.ind <- match(sub[,2L], subcols)
    m.ind <- (c.ind-1)*length(subrows) + r.ind
    if(length(m.ind)!=(length(subrows)+0.0)*length(subcols)) {
      if(is.null(warnMissingValue)) {
        warning("Missing values detected - they are filled with NAs\n")
      }
    }
    mat <- matrix(missingValue, nrow=length(subrows), ncol=length(subcols))
    mat[m.ind] <- sub[,3L]
    rownames(mat) <- subrows
    colnames(mat) <- subcols
    return(mat)
}
