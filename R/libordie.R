libordie <- function(package,
                     minVer,
                     missing.quit.status=1,
                     ver.quit.status=1,
                     ...) {
  package <- as.character(substitute(package))
  if(!package %in% installed.packages()[,1])
    qqmsg("Error: This script requires ", package, " but not found",
          status=missing.quit.status) 
  if(!missing(minVer) && packageVersion(package) < package_version (minVer))
    qqmsg("Error: This script requires ", package, " >=",
          package_version(minVer), " but ",
          packageVersion(package), " was found",
          status=ver.quit.status)
  suppressMessages(library(package,character.only=TRUE,quietly=TRUE, ...))
}
