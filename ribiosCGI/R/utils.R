#' CGI temporary directory
#' @export RIBIOS_CGI_TMP_DIR
RIBIOS_CGI_TMP_DIR <- "/data64/bi/httpd_8080/sawitmp/"

#' CGI temporary URL
#' @export RIBIOS_CGI_TMP_URL
RIBIOS_CGI_TMP_URL <- "http://bioinfo.bas.roche.com:8080/sawitmp/"

#' Get temporary web file
#' @param pattern Character, initial part of the temporary file name
#' @param fileext File name extesion, i.e. \dQuote{.png} (note the dot)
#' @param tmpdir Temporary directory
#'
#' @return Character, the full name of the temporary file
#' @details  When handling web requests, it is sometimes necessary to write
#'  temporary files to the server file system, which is accessible by an
#'  URL. \code{tmpWebFile} returns a temporary file name. The URL can be
#'  extracted from the temp file name by \code{tmpWebURL}.
#'
#'  The default temporary directory and its URL address has been adjusted to the
#'  BIOS installation: \code{RIBIOS_CGI_TMP_DIR} is the temporary
#'  directory, and \code{RIBIOS_CGI_TMP_URL} points to it. It is up to
#'  users to make sure that the directory and the URL points to the same
#'  place in case the values are explicitly specified.
#'
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @examples
#' tmpFile <- tmpWebFile(pattern="file", fileext=".png")
#' print(tmpFile)
#' @export tmpWebFile
tmpWebFile <- function(pattern="file", fileext="", tmpdir=RIBIOS_CGI_TMP_DIR) {
  ## 'fileext' was a feature (at latest) available from R-2.16.0
  if("fileext" %in% names(as.list(args(tempfile)))) {
    return(tempfile(pattern=pattern, tmpdir=tmpdir, fileext=fileext))
  } else {
    return(paste(tempfile(pattern=pattern, tmpdir=tmpdir),
                 fileext, sep=""))
  }
}

#' Get URL pointing to the temporary web file
#' @param tmpFile Full name of the temporary file
#' @param tmpurl URL root of the file
#'
#' @return Character, the URL of the temporary file
#' @author Jitao David Zhang <jitao_david.zhang@roche.com>
#' @examples
#' tmpFile <- tmpWebFile(pattern="file", fileext=".png")
#' print(tmpWebURL(tmpFile))
#'
#' @export tmpWebURL
tmpWebURL <- function(tmpFile, tmpurl=RIBIOS_CGI_TMP_URL) {
  paste(tmpurl,basename(tmpFile), sep="")
}
