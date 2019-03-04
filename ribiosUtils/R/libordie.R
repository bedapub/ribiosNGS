#' Load a library mutedly and quit (die) in case of failing
#' 
#' The specified library is loaded mutedly by suppressing all messages. If the
#' library is not found, or its version under the specification of
#' \code{minVer}, the R session dies with a message.
#' 
#' Only one package should be tested once.
#' 
#' @param package One package name (can be character or non-quoted symbol (see
#' examples)
#' @param minVer Optional, character string, the minimum working version
#' @param missing.quit.status Integer, the status of quitting when the package
#' was not found
#' @param ver.quit.status Integer, the status of quitting when the package was
#' found, but older than the minimum working version
#' @return \code{NULL} if success, otherwise the session will be killed.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso The function calls \code{\link{qqmsg}} internally to kill the
#' session
#' @examples
#' 
#' \dontrun{
#' libordie(stats)
#' libordie("methods")
#' libordie(base, minVer="2.15-1")
#' }
#' 
#' @importFrom utils installed.packages packageDescription
#' @export libordie
libordie <- function(package,
                     minVer,
                     missing.quit.status=1,
                     ver.quit.status=1) {
  package <- as.character(substitute(package))
  if(!package %in% installed.packages()[,1])
    qqmsg("Error: This script requires ", package, " but not found",
          status=missing.quit.status)
  pkgVer <- package_version(packageDescription(package, fields="Version"))
  if(!missing(minVer) && pkgVer < package_version (minVer))
    qqmsg("Error: This script requires ", package, " >=",
          package_version(minVer), " but ",
          pkgVer, " was found",
          status=ver.quit.status)
  suppressMessages(library(package,character.only=TRUE,quietly=TRUE))
}
