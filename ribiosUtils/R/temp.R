#' A temporary directory which (1) every machine in the cluster has access to and (2) has sufficient space
#'
#' @return a character string of the directory name
#' @seealso \code{\link{ribiosTempfile}}
ribiosTempdir <- function() {
    file.path("/data64/bi/tmp/",
              basename(tempdir()))
}

#' A temporary file which (1) every machine in the cluster has access to and (2) there is sufficient space
#'
#' @return a character string of the file name
#' @seealso \code{\link{ribiosTempdir}}

ribiosTempfile <- function(pattern="file",
                           tmpdir=ribiosTempdir(), fileext="") {
    tempfile(pattern=pattern, tmpdir=tmpdir, fileext=fileext)
}
