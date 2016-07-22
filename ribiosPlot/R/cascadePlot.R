unisignCascadeOrder <- function(matrix, decreasing=TRUE) {
    maxtime <- apply(matrix, 1L, function(x) which.max(abs(x)))
    maxval <- apply(matrix, 1L, function(x) max(abs(x)))
    order(maxtime, -maxval, decreasing=FALSE)
}

#' Order rows of a matrix in the cascade order
#'
#' The 'cascade order' is defined by (1) rows are divided into two groups by the condition given by 'dichotomy'
#' (2) the positive and negative rows are ordered respectively so that rows reaching its absolute maximal values
#' in column n are ordered prior to rows reaching reaching its absolute maximal values in columns n+1, where n can
#' be from 1 to column number minus one.
#' 
#' @param matrix A numeric matrix
#' @param dichotomy How are the rows divided into two? By mean value, median value, or by absolute maximal value
#'
#' @examples
#' myMatrix <- matrix(c(1,2,3, 4,6,5, 8,7,9,3,4,8,9,1,2,-1,-2,-3, -4, -2, -1, -5, -7, -4), byrow=TRUE, ncol=3)
#' rownames(myMatrix) <- sprintf("Gene%d", 1:nrow(myMatrix))
#' biosHeatmap(myMatrix, Colv=FALSE, Rowv=FALSE, dendrogram="none")
#' myOrder <- cascadeOrder(myMatrix)
#' myOrderedMatrix <- myMatrix[myOrder,]
#' biosHeatmap(myOrderedMatrix, Colv=FALSE, Rowv=FALSE, dendrogram="none")

cascadeOrder <- function(matrix, dichotomy=c('mean', 'median', 'absmax')) {
    dichotomy <- match.arg(dichotomy)
    if(is.null(row.names(matrix)))
        rownames(matrix) <- 1:nrow(matrix)
    if(dichotomy=="mean") {
        dichfun <- function(x) mean(x, na.rm=TRUE)<=0
    } else if (dichotomy=="median") {
        dichfun <- function(x) median(x, na.rm=TRUE)<=0
    } else if (dichotomy=="absmax") {
        dichfun <- function(x) x[which.max(x)]<=0
    }
    isNeg <- apply(matrix, 1, dichfun)
    isNeg[is.na(isNeg)] <- TRUE
    isPos <- !isNeg
    negMatrix <- matrix[isNeg,]
    posMatrix <- matrix[isPos,]
    posOrder <- unisignCascadeOrder(posMatrix, decreasing=TRUE)
    negOrder <- unisignCascadeOrder(negMatrix, decreasing=FALSE)
    
    ordrn <- c(rownames(matrix)[isPos][posOrder],
               rownames(matrix)[isNeg][negOrder])
    ind <- match(ordrn, rownames(matrix))
    return(ind)
}
