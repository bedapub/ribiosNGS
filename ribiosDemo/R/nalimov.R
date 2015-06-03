nalimov <- function(x, sig=c("0.95", "0.99", "0.995")) {
  sig <- match.arg(sig)
  if(sig=="0.95") {
    isig <- 0L
  } else if (sig=="0.99") {
    isig <- 1L
  } else {
    isig <- 2L
  }
  .Call("bios_nalimov", as.numeric(x), isig)
}
