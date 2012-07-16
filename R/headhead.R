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
