#' Prepare the environment for a script
#' @aliases initScript
#' 
#' This function is called at the beginning of an Rscript, in order to
#' prepare the R environment to run in a script setting.
#'
#' @return Only side effect is used
#' 
#' @export
#' @examples
#' \dontrun{
#'   scriptInit()
#' }
scriptInit <- function() {
  if(interactive()) {
    setDebug()
  } else {
    options(error=quote({
      dump.frames(dumpto="ribios.dump", to.file=TRUE, include.GlobalEnv=TRUE)
      quit(save="no", status=1L)
    }))
  }
}
initScript <- scriptInit
