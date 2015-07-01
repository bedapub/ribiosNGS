RIBIOS_CGI_TMP_DIR <- "/data64/bi/httpd_8080/sawitmp/"
RIBIOS_CGI_TMP_URL <- "http://bioinfo.bas.roche.com:8080/sawitmp/"

tmpWebFile <- function(pattern="file", fileext="", tmpdir=RIBIOS_CGI_TMP_DIR) {
  ## 'fileext' was a feature (at latest) available from R-2.16.0
  if("fileext" %in% names(as.list(args(tempfile)))) {
    return(tempfile(pattern=pattern, tmpdir=tmpdir, fileext=fileext))
  } else {
    return(paste(tempfile(pattern=pattern, tmpdir=tmpdir),
                 fileext, sep=""))
  }
}
tmpWebURL <- function(tmpFile, tmpurl=RIBIOS_CGI_TMP_URL) {
  paste(tmpurl,basename(tmpFile), sep="")
}
