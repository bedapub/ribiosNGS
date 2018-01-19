cossim <- function(x,y, na.rm=TRUE) {
  if(missing(y)) y <- x
  xIsMatrix <- is.matrix(x)
  yIsMatrix <- is.matrix(y)
  if(!xIsMatrix && !yIsMatrix) {
    return(.Call(C_cossim, x, y, na.rm))
  } else if(xIsMatrix && !yIsMatrix) {
    y <- matrix(y, ncol=1L)
  } else if(!xIsMatrix && yIsMatrix) {
    x <- matrix(x, ncol=1L)
  }
  if(nrow(x)!=nrow(y))
    stop("incompatible dimension")

  aa <- t(x) %*% y
  bb <- apply(x, 2L, function(xx) sqrt(sum(xx^2)))
  cc <- rep(apply(y, 2L, function(yy) sqrt(sum(yy^2))),
            each=nrow(aa))
  res <- aa/bb/cc
  if(! (xIsMatrix && yIsMatrix))
    res <- as.vector(res)
  return(res)
}

cosdist <- function(x,y, na.rm=TRUE) {
  1-cossim(x=x, y=y, na.rm=na.rm)
}
