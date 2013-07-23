RIBIOS_CGI_TMP_DIR <- "/DATA/bi/httpd_8080/htdoc/sawitmp/"
RIBIOS_CGI_TMP_URL <- "http://bioinfo.bas.roche.com:8080/sawitmp/"

tmpWebFile <- function(pattern="file", fileext="", tmpdir=RIBIOS_CGI_TMP_DIR) {
  tempfile(pattern=pattern, tmpdir=tmpdir, fileext=fileext)
}
tmpWebURL <- function(tmpFile, tmpurl=RIBIOS_CGI_TMP_URL) {
  paste(tmpurl,basename(tmpFile), sep="")
}
