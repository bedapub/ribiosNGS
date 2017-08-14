#' Prepare the environment for a script
#' @aliases initScript
#' 
#' This function is called at the beginning of an Rscript, in order to
#' prepare the R environment to run in a script setting.
#'
#' @param dumpto Character string, file name to dump to when there is an error
#' @param include.GlobalEnv logical indicating if a _copy_ of the .GlobalEnv enrivonment should be included
#' in addition tot he 'sys.frames()'. Useful when used in a batch job
#' 
#' @return Only side effect is used
#' 
#' @export
#' @examples
#' \dontrun{
#'   scriptInit()
#' }
scriptInit <- function(dumpto="ribios.dump", include.GlobalEnv=TRUE) {
  if(interactive()) {
    setDebug()
  } else {
    options(error=quote({
      dump.frames(dumpto, to.file=TRUE, include.GlobalEnv=include.GlobalEnv)
      quit(save="no", status=1L)
    }))
  }
}
initScript <- scriptInit
