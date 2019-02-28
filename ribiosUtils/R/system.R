#' Quietly runs a system command
#' 
#' Quietly runs a system command: the output is internalized and returned as an
#' invisible variable, and the standard error output is ignored.
#' 
#' The function runs the system command in a quiet mode. The function can be
#' useful in CGI scripts, for instance
#' 
#' @param command A system command
#' @return (Invisibly) the internalized output of the command
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' dateIntern <- system("date")
#' 
#' @export qsystem
qsystem <- function(command) {
  invisible(system(command,intern=TRUE, ignore.stderr=TRUE))
}

#' Remove temporary files at a specified time interval from now
#' 
#' Send a \code{at} job to remove (probably temporary) files in the future with
#' a specified time interval from now
#' 
#' The command will delete files, and there is usually no way to get deleted
#' files back. \emph{So make sure you know what you are doing!}
#' 
#' Days, hours, and minutes can be given in a mixed way: they will be summed up
#' to give the interval.
#' 
#' @param \dots Files to be removed
#' @param days Numeric, interval in days
#' @param hours Numeric, interval in hours
#' @param minutes Numeric, interval in minutes
#' @param dry Logical, if set to \code{TRUE}, only the command will be returned
#' and files are not really removed.
#' @return (Invisibly) the output of the \code{at} job.
#' @note Since the command uses \code{at} internally, it is unlikely the
#' command will work in the Windows system \dQuote{out of box}.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{qsystem}} for running system commands quietly.
#' @examples
#' 
#' tmp1 <- tempfile()
#' tmp2 <- tempfile()
#' rmat(tmp1, tmp2, minutes=1)
#' 
#' @export rmat
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
