haltifnot <- function(...,
                      msg="Error undefined. Please contact the developer") {
  n <- length(ll <- list(...))
  if (n == 0L)
    return(invisible())
  mc <- match.call()
  for (i in 1L:n) if (!(is.logical(r <- ll[[i]]) && !any(is.na(r)) &&
                        all(r))) {
    ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
    if (length(ch) > 1L)
      ch <- paste(ch[1L], "....")
    stop(msg, call. = FALSE)
  }
  invisible()
}
