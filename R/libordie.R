## The libordie function has been tested to run on both R-2.11.1 and R-2.15.0
libordie <- function(package,
                     minVer,
                     missing.quit.status=1,
                     ver.quit.status=1,
                     ...) {
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
  ## quietly was a feature (at latest) available from R-2.13.0
  canQuiet <- "quietly" %in% names(as.list(args(library)))
  if(canQuiet)
    suppressMessages(library(package,character.only=TRUE,quietly=TRUE, ...))
  else
    suppressMessages(library(package,character.only=TRUE,...))
}
