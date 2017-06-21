loadFile <- function (rDataFile, env = globalenv()) {
    if (file.exists(rDataFile)) {
        load(rDataFile, env)
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}

#' Load an object by its name from a RData file
#' @param file A RData file
#' @param obj Object name. If set as \code{NULL}, all objects are returned
#' @param verbose Whether the loading process should be verbose, see \code{\link{load}}
#'
#' @export
loadObject <- function(file, obj=NULL, verbose=FALSE) {
  env <- new.env()
  load(file, env, verbose = verbose)
  if(is.null(obj))
     obj <- ls(env=env)
  get(obj, env)
}
