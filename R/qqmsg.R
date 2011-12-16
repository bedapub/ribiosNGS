qqmsg <- function(..., status=0, save=FALSE, runLast=TRUE) {
  message(...)
  ss <- ifelse(save, "yes", "no")
  quit(save=ss, status=status, runLast=runLast)
}
