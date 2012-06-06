isInteractiveDebugging <- function() isDebugging() || interactive()
isDebugging <- function() Sys.getenv("RIBIOS_SCRIPT_DEBUG") != ""
setDebug <- function() Sys.setenv(RIBIOS_SCRIPT_DEBUG="TRUE")
unsetDebug <- function() Sys.unsetenv("RIBIOS_SCRIPT_DEBUG")
