#' Multiple identical
#' 
#' Testing whether multiple objects are identical
#' 
#' \code{midentical} extends \code{identical} to test multiple objects instead
#' of only two.
#' 
#' @param \dots Objects to be tested, or a list of them
#' @param num.eq,single.NA,attrib.as.set,ignore.bytecode, See
#' \code{\link{identical}}
#' @param ignore.environment,ignore.srcref See \code{\link{identical}}
#' @return A logical value, \code{TRUE} if all objects are identical
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{identical}
#' @examples
#' 
#' set1 <- "HSV"
#' set2 <- set3 <- set4 <- c("HSV", "FCB")
#' 
#' midentical(set1, set2)
#' midentical(list(set1, set2))
#' 
#' midentical(set2, set3, set4)
#' midentical(list(set2, set3, set4))
#' 
#' ## other options passed to identical
#' midentical(0, -0, +0, num.eq=FALSE)
#' midentical(0, -0, +0, num.eq=TRUE)
#' 
#' @export midentical
midentical <- function(..., num.eq=TRUE, single.NA=TRUE, 
                       attrib.as.set=TRUE,
                       ignore.bytecode=TRUE, 
                       ignore.environment=FALSE,
                       ignore.srcref=TRUE) {
  li <- list(...)
  if(length(li)==1L) li <- li[[1L]]
  
  stopifnot(length(li)>=2L)
  identical.formals <- names(formals(identical))
  if(!identical(identical.formals,
                c("x", "y", "num.eq", "single.NA",
                  "attrib.as.set",
                  "ignore.bytecode", "ignore.environment", "ignore.srcref"))) {
      stop("formals of identical have changed, please consult the developer")
  }
  call.list <- list(x=li[[1L]], y=li[[2L]],
                    num.eq=num.eq, single.NA=single.NA,
                    attrib.as.set=attrib.as.set,
                    ignore.bytecode=ignore.bytecode,
                    ignore.environment=ignore.environment,
                    ignore.srcref=ignore.srcref)[identical.formals]
  res <- do.call(identical, call.list)
  if(length(li)>2L)
    for(i in 2L:(length(li)-1L)) {
      call.list <- list(x=li[[i]], y=li[[i+1L]],
                        num.eq=num.eq, single.NA=single.NA,
                        attrib.as.set=attrib.as.set,
                        ignore.bytecode=ignore.bytecode,
                        ignore.environment=ignore.environment,
                        ignore.srcref=ignore.srcref)[identical.formals]
      res <- res && do.call(identical, call.list)
      if(!res) return(FALSE)
    }
  return(res)
}
