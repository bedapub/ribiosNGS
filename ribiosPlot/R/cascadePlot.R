#' Variance of features in rows
#' 
#' @param x Numeric matrix
#' @param na.rm Logical. Should missing values (including NaN) be omitted from the calculations?
#'
#' @examples
#' myVal <- matrix(1:9, nrow=3, byrow=FALSE)
#' myVar <- rowVars(myVal)
#' stopifnot(identical(myVar, c(9,9,9)))
#' @keywords internal
rowVars <- function (x, na.rm=TRUE) {
  sqr = function(x) x * x
  n = rowSums(!is.na(x))
  n[n <= 1] = NA
  return(rowSums(sqr(x - rowMeans(x, na.rm=na.rm)), na.rm=na.rm)/(n - 1))
}

#' Order rows of a matrix with non-negative maximum values in the cascade order
#' 
#' @param matrix A matrix with non-negative maximum values of rows. 
#' 
#' The function is internally called by \code{\link{cascadeOrder}} to order the rows of the matrix with non-negative maximum values.
#' The function works by ordering the rows by peaks (in which column does the value peak?) and then by maximum values (what are the maximum values?).
#' In case of invariant rows and rows of NAs, they are put at last.
#'
#' If maximum values of the rows are negative, negate the matrix before passing it to the matrix (see \code{\link{cascadeOrder}}).
#' @keywords internal
nonNegCascadeOrder <- function(matrix) {
  rowInd <- 1:nrow(matrix)
  naRows <- apply(matrix, 1, function(x) all(is.na(x)))
  if(any(naRows)) {
    naInd <- which(naRows)
    matrix <- matrix[!naRows,, drop=FALSE]
    rowInd <- rowInd[!naRows]
  }
  invarRows <- rowVars(matrix) == 0
  if(any(invarRows)) {
    invarInd <- which(invarRows)
    matrix <- matrix[!invarRows,, drop=FALSE]
    rowInd <- rowInd[!invarRows]
  }
  timeSort <- t(apply(matrix, 1, function(x) {
    valOrd <- order(x, decreasing=TRUE, na.last=TRUE)
    valSort <- -x[valOrd]
    res <- numeric(length(x)*2)
    res[seq(1,length(res), 2)] <- valOrd
    res[seq(2,length(res), 2)] <- valSort
    return(res)
  }))
  timeSortOrd <- do.call(order,
                 as.data.frame(timeSort))
  res <- rowInd[timeSortOrd]
  if(any(invarRows)) {
    res <- c(res, invarInd)
  }
  if(any(naRows)) {
    res <- c(res, naInd)
  }
  return(res)
}

#' Order rows of a matrix in the cascade order
#'
#' The 'cascade order' is defined by three criteria (1) Rows are divided into two groups by the condition given by 'dichotomy'.
#' (2) The positive and negative rows are ordered respectively so that rows reaching its absolute maximal values
#' in column n are ordered prior to rows reaching reaching its absolute maximal values in columns n+1, where n can
#' be from 1 to column number minus one. (3) If two rows reach the maximum value at the same column, they 
#' are ordered by the (decreasing) order of the absolute value in that column.
#' 
#' See example for illustration of the idea.
#' 
#' @param matrix A numeric matrix
#' @param dichotomy How are the rows divided into two? By maximal abs value (default), mean value, or the median value of each row.
#'
#' @examples
#' checkBoard <- function(seed=1887) {
#'   set.seed(seed)
#'   mat <- matrix(rnorm(76, sd=1), ncol=4)
#'   delta <- 3
#'   for(i in seq(1, 16, 2)) {
#'     rowInd <- i:(i+1)
#'     colInd <- (i %/% 2) %% 4 +1
#'     delta <- ifelse(i>8, -6, 6) * c(0.6, 1)
#'     mat[rowInd, colInd] <- mat[rowInd, colInd] + delta
#'   }
#'   mat[17,1:4] <- rep(-1, 4)
#'   mat[18,1] <- NA
#'   mat[18,2] <- mat[18, 2] -6
#'   mat[19,1:4] <- rep(NA,4)
#'   rord <- sample(1:nrow(mat), replace=FALSE)
#'   mat <- mat[rord,]
#'   rownames(mat) <- sprintf("Row%d", 1:nrow(mat))
#'   return(mat)
#' }
#' myMat <- checkBoard(1887)
#' biosHeatmap(myMat, Rowv=FALSE, Colv=FALSE, dendrogram="none",
#'             zlim=c(-4,4), col="royalbluered",
#'             main="Original matrix")
#' ## since dist by default does not accept rows full of NAs, we remove them in the example below
#' biosHeatmap(myMat[apply(myMat, 1, function(x) !all(is.na(x))),],
#'             Rowv=TRUE, Colv=TRUE, dendrogram="both",
#'             zlim=c(-4,4), col="royalbluered",
#'             main="hclust/dist clustering")
#' ## note that cascadeOrder handles invariant rows and rows full of NA values
#' biosHeatmap(myMat[cascadeOrder(myMat),], Rowv=FALSE, Colv=FALSE, dendrogram="none",
#'             zlim=c(-4,4), col="royalbluered",
#'             main="Cascade order")
cascadeOrder <- function(matrix, dichotomy=c('maxabs', 'mean', 'median')) {
    dichotomy <- match.arg(dichotomy)
    if(is.null(row.names(matrix)))
        rownames(matrix) <- 1:nrow(matrix)
    if(dichotomy=="mean") {
        dichfun <- function(x) mean(x, na.rm=TRUE)<=0
    } else if (dichotomy=="median") {
        dichfun <- function(x) median(x, na.rm=TRUE)<=0
    } else if (dichotomy=="maxabs") {
        dichfun <- function(x) {
            if(all(is.na(x))) {
                return(TRUE)
            } else {
                return(x[which.max(abs(x))]<=0)
            }
        }
    }
    isNeg <- apply(matrix, 1, dichfun)
    isNeg[is.na(isNeg)] <- TRUE
    isPos <- !isNeg
    negMatrix <- matrix[isNeg,,drop=FALSE]
    posMatrix <- matrix[isPos,,drop=FALSE]
    posOrder <- nonNegCascadeOrder(posMatrix)
    negOrder <- nonNegCascadeOrder(-negMatrix)
    
    ordrn <- c(rownames(matrix)[isPos][posOrder],
               rownames(matrix)[isNeg][negOrder])
    ind <- match(ordrn, rownames(matrix))
    return(ind)
}

