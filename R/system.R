qsystem <- function(command) {
  invisible(system(command,intern=TRUE, ignore.stderr=TRUE))
}

rmat <- function(..., days=NULL, hours=NULL, minutes=NULL, dry=TRUE) {
  count=0.0;
  if(!is.null(days) && !is.na(nday <- as.numeric(days)))
    count=count + 1440 * nday
  if(!is.null(hours) && !is.na(nhour <- as.numeric(hours)))
    count=count + 60 * nhour
  if(!is.null(minutes) && !is.na(nmin <- as.numeric(minutes)))
    count=count + nmin
  files <- paste(as.character(unlist(list(...))), collapse=" ")
  comm <- sprintf("echo rm -f %s | at now + %.0f minutes",
                  files, count)
  if(dry) {
    return(comm)
  } else {
    return(qsystem(comm))
  }
}
