#' Quitely Quit with Messages
#' 
#' Quitely quit R with messages in non-interactive sessions
#' 
#' The function prints messages in any case, and quits R if the current session
#' is non-interactive, e.g. in the command-line running Rscript mode
#' 
#' @param \dots Messages to be passed to \code{message}
#' @param status Quit stats
#' @param save Logical, should current working environment be saved?
#' @param runLast Logical, should \code{.Last()} be executed?
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{quit}}
#' @examples
#' 
#' \dontrun{
#' qqmsg()
#' qqmsg("die", status=0)
#' qqmsg("Avada kedavra", status=-1)
#' qqmsg("Crucio!", "\n", "Avada kedavra", status=-100)
#' }
#' 
#' @export qqmsg
qqmsg <- function(..., status=0, save=FALSE, runLast=TRUE) {
  if(length(list(...))>0)
    message(...)
  ss <- ifelse(save, "yes", "no")
  if(!interactive())
    quit(save=ss, status=status, runLast=runLast)
}
