#' Repeat garbage-collecting until all resource is freed
#' 
#' stubborngc repeats collecting garbage untill no more resource can be freed
#' 
#' 
#' @param verbose Logical, verbose or not
#' @param reset Logical, reset or not.
#' @return Side effect is used.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{gc}}
#' @keywords ~kwd2
#' @examples
#' 
#' stubborngc()
#' 
#' @export stubborngc
stubborngc <- function(verbose=FALSE, reset=TRUE) {
  .local <- function() gc(verbose=verbose, reset=reset)[2,3]
  gct <- .local()
  while(gct!=.local()) {
    gct <- .local()
  }
}
