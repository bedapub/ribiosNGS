#' Returns the file name of the Rscript being executed
#'
#' Get the file name of the Rscript that is currently being executed. The function is mainly called by stand-alone Rscripts.
#'
#' @details The name is determined by the \code{--file}/\code{-f} option in the command line.
#'
#' When the R session was not initiated by a Rscript (i.e. there is no \code{--file} or \code{-f} option in the command line), \code{NULL} is returned.
#'
#' Note that the function supports calling Rscript via \code{--file}  or \code{-f} with \code{R}. This applies to cases where a Rscript, marked as executable, and is called from the command line.
#'
#' @return  A character string containing the file name of the Rscript.
#'
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{commandArgs}} and \code{\link{getArg}}
#'
#' @examples
#' \dontrun{scriptName()}
#'
scriptName <- function() {
  filename <- grep("--file=", commandArgs(), value=TRUE)
  lf <- length(filename)
  if(lf>1) {
    warning("Multiple --file options found, only using the first")
    filename <- filename[1L]
  } else if (lf==1) {
    restname <- strsplit(filename, "=")[[1L]][-1L]
    return(paste(restname, collapse="="))
  } else if (lf==0) { ## no --file found
    filename <- getArg("f", onlyArg=NULL, missingArg=NULL)
    return(filename) ## if missing: NULL, otherwise: the file name
  }
}

#' Returns the path of the Rscript being executed
#'
#' Get the normalised path of the Rscript that is currently being executed. The function is mainly called by stand-alone Rscripts.
#'
#' @details The name is determined by the \code{--file}/\code{-f} option in the command line.
#'
#' When the R session was not initiated by a Rscript (i.e. there is no \code{--file} or \code{-f} option in the command line), \code{NULL} is returned.
#'
#' Note that the function supports calling Rscript via \code{--file}  or \code{-f} with \code{R}. This applies to cases where a Rscript, marked as executable, and is called from the command line.
#'
#' @return  A character string containing the normalised path of the Rscript.
#'
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @seealso \code{\link{scriptName}}
#'
#' @examples
#' \dontrun{scriptPath()}
#'
scriptPath <- function() {
  sname <- scriptName()
  if(is.null(sname)) {
    return(NULL)
  } else {
    filename <- normalizePath(sname)
    return(dirname(filename))
  }
}
