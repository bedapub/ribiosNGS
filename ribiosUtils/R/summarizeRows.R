apply2 <- function(x, fun=mean, ...) {
  if(nrow(x)==1)
    return(x)
  else
    return(apply(x, 2, fun, ...))
}

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

apply1 <- function(x, fun=mean, ...) {
  if(ncol(x)==1)
    return(x)
  else
    return(apply(x, 1, fun, ...))
}


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

summarizeCols <- summarizeColumns
