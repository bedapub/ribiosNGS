stubborngc <- function(verbose=FALSE, reset=TRUE) {
  .local <- function() gc(verbose=verbose, reset=reset)[2,3]
  gct <- .local()
  while(gct!=.local()) {
    gct <- .local()
  }
}
