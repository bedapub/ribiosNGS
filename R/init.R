scriptInit <- function() {
  options(error=quote({dump.frames("ribios.dump", to.file=TRUE); quit(save="no", status=1L)}))
}
