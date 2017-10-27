libordie <- function(package,
                     minVer,
                     missing.quit.status=1,
                     ver.quit.status=1, ...) {
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
  suppressMessages(library(package,character.only=TRUE,quietly=TRUE, ...))
}
