#' @export apply2
apply2 <- function(x, fun=mean, ...) {
  if(nrow(x)==1)
    return(x)
  else
    return(apply(x, 2, fun, ...))
}





#' Summarizing rows/columns by a factor
#' 
#' Apply a function to summarize rows/columns that assigned to the same level
#' by a factor vector.
#' 
#' \code{NA} levels are neglected, and corresponding rows/columns will not
#' contribute to the summarized matrix.
#' 
#' \code{summarizeCols} is synonymous to \code{summarizeColumns}
#' 
#' @aliases apply1 apply2 summarizeRows summarizeCols summarizeColumns
#' @param matrix A numeric matrix
#' @param factor A vector of factors, either of the length of
#' \code{nrow(matrix)} (for \code{summarizeRows}), or the length of
#' \code{ncol(matrix)} (for \code{summarizeColumns}).
#' @param fun A function or a name for a function, the summarizing function
#' applied to rows/columns sharing the same level
#' @param ... Further parameters passed to the function
#' @return A matrix, the dimension will be determined by the number of levels
#' of the factor vector.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' my.matrix <- matrix(1:25, nrow=5)
#' print(my.matrix)
#' 
#' my.factor <- factor(c("A", "B", "A", "C", "B"))
#' summarizeRows(matrix=my.matrix, factor=my.factor, fun=mean)
#' summarizeRows(matrix=my.matrix, factor=my.factor, fun=prod)
#' summarizeColumns(matrix=my.matrix, factor=my.factor, fun=mean)
#' summarizeColumns(matrix=my.matrix, factor=my.factor, fun=prod)
#' 
#' ## NA values in factor
#' my.na.factor <- factor(c("A", "B", "A", "C", NA))
#' summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=mean)
#' summarizeRows(matrix=my.matrix, factor=my.na.factor, fun=prod)
#' summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=mean)
#' summarizeColumns(matrix=my.matrix, factor=my.na.factor, fun=prod)
#' 
#' @export summarizeRows
summarizeRows <- function(matrix,
                          factor,
                          fun=mean, ...) {
  stopifnot(is.matrix(matrix) && nrow(matrix) == length(factor))
  fun <- match.fun(fun)
  ind.by.fac <- split(1:nrow(matrix), factor)
  mat.fun <- t(sapply(ind.by.fac,
                      function(x) apply2(matrix[x,,drop=FALSE],
                                         fun=fun, ...)))
  colnames(mat.fun) <- colnames(matrix)
  return(mat.fun)
}

#' @export apply1
apply1 <- function(x, fun=mean, ...) {
  if(ncol(x)==1)
    return(x)
  else
    return(apply(x, 1, fun, ...))
}


#' @export summarizeColumns
summarizeColumns <- function(matrix,
                             factor,
                             fun=mean, ...) {
  stopifnot(is.matrix(matrix) && ncol(matrix) == length(factor))
  fun <- match.fun(fun)
  ind.by.fac <- split(1:ncol(matrix), factor)
  mat.fun <- sapply(ind.by.fac,
                    function(x) apply1(matrix[,x,drop=FALSE],
                                       fun=fun, ...))
  rownames(mat.fun) <- rownames(matrix)
  return(mat.fun)
}

#' @export summarizeCols
summarizeCols <- summarizeColumns
