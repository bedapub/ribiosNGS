qqmsg <- function(..., status=0, save=FALSE, runLast=TRUE) {
  if(length(list(...))>0)
    message(...)
  ss <- ifelse(save, "yes", "no")
  if(!interactive())
    quit(save=ss, status=status, runLast=runLast)
}
