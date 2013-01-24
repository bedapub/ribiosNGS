scriptInit <- function(dumpto="ribios.dump") {
  options(error=quote({dump.frames(dumpto=dumpto, to.file=TRUE); q()}))
}
