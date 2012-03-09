verbose <- function(..., global=1L, this=1L) {
  if(global >= this)
    message(...)
}
