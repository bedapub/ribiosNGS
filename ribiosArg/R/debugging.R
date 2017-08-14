#' Test whether the environment is set for debugging
#'
#' @return A logical value
#' @seealso \code{\link{setDebug}} and \code{\link{unsetDebug}}
#' 
#' @examples
#' isDebugging()
#' unsetDebug()
#' isDebugging()
#' setDebug()
#'
#' @export
isDebugging <- function() Sys.getenv("RIBIOS_SCRIPT_DEBUG") != ""

#' Test whether the environment is set for debugging, or it's an interactive session
#'
#' @return A logical value
#' @seealso \code{\link{isDebugging}}
#'
#' @export
isIntDebugging <- function() isDebugging() || interactive()

#' Set the enrivonment for debugging
#'
#' @return A logical value, whether the setting was susccessful or not
#' @seealso \code{\link{isDebugging}} and \code{\link{unsetDebug}}
#' @export 
setDebug <- function() Sys.setenv(RIBIOS_SCRIPT_DEBUG="TRUE")

#' Remove the debugging flag of the the enrivonment
#'
#' @return A logical value, whether the removal was successful or not
#' @seealso \code{\link{isDebugging}} and \code{\link{setDebug}}
#' @export
unsetDebug <- function() Sys.unsetenv("RIBIOS_SCRIPT_DEBUG")


rscriptSkeleton <- function(file=stdout) {
   backbone <- "#!/usr/bin/env Rscript"
   suppressMessages()
}
