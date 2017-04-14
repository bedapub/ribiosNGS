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
#' @param obj Object name
#' @param verbose Whether the loading process should be verbose, see \code{\link{load}}
#'
#' @export
loadObject <- function(file, obj, verbose=FALSE) {
    env <- new.env()
    load(file, env, verbose=verbose)
    get(obj, env)
}
