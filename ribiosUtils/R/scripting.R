#' @export isIntDebugging
isIntDebugging <- function() isDebugging() || interactive()

#' @export isDebugging
isDebugging <- function() Sys.getenv("RIBIOS_SCRIPT_DEBUG") != ""




#' Functions for command-line Rscript debugging
#' 
#' These functions are used to debug command-line executable Rscripts in R
#' sessions
#' 
#' \code{setDebug} sets the environmental variable \code{RIBIOS_SCRIPT_DEBUG}
#' as \code{TRUE}. \code{unsetDebug} unsets the variable. \code{isDebugging}
#' checks whether the variable is set or not. \code{isIntDebugging} tests
#' whether the scripts runs interactively or runs in the debugging mode. The
#' last one can be useful when debugging Rscript in a R session.
#' 
#' A programmer wishing to debug a Rscript can explicitly set (or unset) the
#' \code{RIBIOS_SCRIPT_DEBUG} variable in order to activate (inactivate)
#' certain trunks of codes. This can be automated via \code{isDebugging}, or
#' probably more conveniently, by \code{isIntDebugging}: if the script runs in
#' an interactive mode, or the debugging flag is set, the function returns
#' \code{TRUE}.
#' 
#' @aliases setDebug unsetDebug isDebugging isIntDebugging
#' @return \code{setDebug} and \code{unsetDebug} returns an invisible value
#' indicating whether the variable setting (unsetting) was successful.
#' 
#' \code{isDebugging} and \code{isIntDebugging} returns logical values.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @examples
#' 
#' unsetDebug()
#' print(isDebugging())
#' setDebug()
#' print(isDebugging())
#' unsetDebug()
#' print(isDebugging())
#' print(isIntDebugging())
#' 
#' @export setDebug
setDebug <- function() Sys.setenv(RIBIOS_SCRIPT_DEBUG="TRUE")

#' @export unsetDebug
unsetDebug <- function() Sys.unsetenv("RIBIOS_SCRIPT_DEBUG")

#' Prepare R for an interactive script
#' 
#' The function prepares R for an interactive session (e.g. in a script).
#' Currently it defines behaviour in case of errors: a file named
#' \dQuote{ribios.dump} is written.
#' 
#' @return Side effect is used.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{options}}
#' @examples
#' 
#' \dontrun{
#' scriptInit()
#' }
#' 
#' @export scriptInit
scriptInit <- function() {
  if(interactive()) {
    setDebug()
  } else {
    options(error=quote({
      dump.frames("ribios.dump", to.file=TRUE);
      quit(save="no", status=1L)
    }))
  }
}
