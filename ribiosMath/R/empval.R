empval <- function(stat, sim) {
  .Call(C_empval, stat, sim)
}
