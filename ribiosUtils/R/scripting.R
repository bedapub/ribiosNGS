isIntDebugging <- function() isDebugging() || interactive()
isDebugging <- function() Sys.getenv("RIBIOS_SCRIPT_DEBUG") != ""
setDebug <- function() Sys.setenv(RIBIOS_SCRIPT_DEBUG="TRUE")
unsetDebug <- function() Sys.unsetenv("RIBIOS_SCRIPT_DEBUG")
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
