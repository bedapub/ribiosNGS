qsystem <- function(command) {
  invisible(system(command,intern=TRUE, ignore.stderr=TRUE))
}

rmat <- function(..., days=NULL, hours=NULL, minutes=NULL, dry=TRUE) {
  seconds=0.0;
  if(!is.null(days) && !is.na(nday <- as.numeric(days)))
    seconds=seconds + 86400 * nday
  if(!is.null(hours) && !is.na(nhour <- as.numeric(hours)))
    seconds=seconds + 3600 * nhour
  if(!is.null(minutes) && !is.na(nday <- as.numeric(minutes)))
    seconds=seconds + 60 * nday
  files <- paste(as.character(unlist(list(...))), collapse=" ")
  comm <- sprintf("echo rm -rf %s | at now + %.0f seconds",
                  files, seconds)
  if(dry) {
    return(comm)
  } else {
    return(qsystem(comm))
  }
}
