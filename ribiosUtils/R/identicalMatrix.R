#' Test whether two matrices have the same numerica values given certain
#' accuracy
#' 
#' 
#' @param x a matrix
#' @param y another matrix
#' @param epsilon accuracy threshold: absolute differences below this threshold
#' is ignored
#' @return Logical
#' @examples
#' 
#' set.seed(1887); x <- matrix(rnorm(1000), nrow=10)
#' set.seed(1887); y <- matrix(rnorm(1000), nrow=10)
#' set.seed(1882); z <- matrix(rnorm(1000), nrow=10)
#' stopifnot(identicalMatrixValue(x,y))
#' stopifnot(!identicalMatrixValue(x,y+1E-5))
#' stopifnot(!identicalMatrixValue(x,y-1E-5))
#' stopifnot(!identicalMatrixValue(x,z))
#' 
#' @export identicalMatrixValue
identicalMatrixValue <- function(x, y, epsilon=1E-12) {
  all(abs(x - y) < epsilon)
}

#' Test whether two matrices are identical by values and by dim names
#' 
#' 
#' @param x a matrix
#' @param y another matrix
#' @param epsilon accuracy threshold: absolute differences below this threshold
#' is ignored
#' @return Logical
#' @examples
#' 
#' set.seed(1887); x <- matrix(rnorm(1000), nrow=10, dimnames=list(LETTERS[1:10],NULL))
#' set.seed(1887); y <- matrix(rnorm(1000), nrow=10, dimnames=list(LETTERS[1:10],NULL))
#' set.seed(1887); z <- matrix(rnorm(1000), nrow=10, dimnames=list(letters[1:10],NULL))
#' stopifnot(identicalMatrix(x,y))
#' stopifnot(!identicalMatrix(x,z))
#' 
#' @export identicalMatrix
identicalMatrix <- function(x, y, epsilon=1E-12) {
  identical(dimnames(x), dimnames(y)) & identicalMatrixValue(x,y)
}
