#' Print head and tail elements of a vector
#' 
#' This function prints head and tail elements of a vector for visualization
#' purposes. See examples for its usage.
#' 
#' Head and tail elements are concatenated with ellipsis, if there are any
#' elements that are not shown in the vector.
#' 
#' @param vec A vector of native types (e.g. character strings)
#' @param head Integer, number of head elements to be printed
#' @param tail Integer, number of tail elements to be printed
#' @param collapse Character string, used to collapse elements
#' @return A character string representing the vector
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{head}}, \code{\link{tail}}
#' @examples
#' 
#' testVec1 <- LETTERS[1:10]
#' headtail(testVec1)
#' headtail(testVec1, head=3, tail=3)
#' headtail(testVec1, head=3, tail=3, collapse="|")
#' 
#' testVec2 <- letters[1:3]
#' headtail(testVec2, head=1, tail=1)
#' headtail(testVec2, head=2, tail=1)
#' 
#' @export headtail
headtail <- function(vec, head=2, tail=1, collapse=", ") {
  vec <- as.character(vec)
  if(length(vec)<=(head+tail)) {
    nvec <- vec
  } else {
    nvec <- c(vec[1:head], "...", vec[(length(vec)-tail+1):length(vec)])
  }
  return(paste(nvec, collapse=collapse))
}





#' head/tail function for matrix or data.frame
#' 
#' These two functions reassembles \code{head} and \code{tail}, showing the
#' first rows and columns of 2D data structures, e.g. matrix or data.frame.
#' 
#' While \code{head} and \code{tail} can be applied to \code{data.frame} or
#' \code{matrix} as well, they show all columns of the first (last) rows even
#' if the matrix has a large number of columns. These two function,
#' \code{headhead} and \code{tailtail}, circumvent this problem by showing only
#' the first rows AND the first columns.
#' 
#' @aliases headhead tailtail
#' @param x A \code{data.frame} or \code{matrix}
#' @param m Integer, number of rows to show
#' @param n Integer, number of columns to show
#' @return The first rows/columns of the input object
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{head}}, \code{\link{tail}}
#' @examples
#' 
#' myMat <- matrix(rnorm(10000), nrow=10L)
#' \dontrun{
#'   head(myMat)
#' }
#' headhead(myMat)
#' tailtail(myMat)
#' 
#' @export headhead
headhead <- function(x, m=6L, n=6L) {
  stopifnot(length(n) == 1L && length(m) == 1L)
  n <- ifelse(n<0L,
              pmax(ncol(x)+n, 0L),
              pmin(n, ncol(x)))
  m <- ifelse(m<0L,
              pmax(nrow(x)+m, 0L),
              pmin(m, nrow(x)))
  
  x[seq_len(m), seq_len(n), drop = FALSE]
}

#' @export tailtail
tailtail <- function(x, m = 6L, n = 6L) {
  stopifnot(length(m) == 1L & length(n) == 1L)
  mrx <- nrow(x)
  ncx <- ncol(x)
  m <- ifelse(m<0L,
              pmax(mrx+m, 0L),
              pmin(m, mrx))
  n <- ifelse(n<0L,
              pmax(ncx+n, 0L),
              pmin(n, ncx))
  
  sel.row <- seq.int(to = mrx, length.out = m)
  sel.col <- seq.int(to = ncx, length.out = n)
  
  ans <- x[sel.row, sel.col, drop = FALSE]
  if (is.null(rownames(x))) rownames(ans) <- paste("[", sel.row, ",]", sep = "")
  if (is.null(colnames(x))) colnames(ans) <- paste("[", sel.col, ",]", sep = "")
  ans
}
