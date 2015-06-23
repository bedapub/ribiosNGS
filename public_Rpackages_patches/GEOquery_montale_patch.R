## FTP mode patch for GEOquery (v2.13.5) on montale
## author: zhangj83, April 2011

library(RCurl)
getAndParseGSEMatrices <- function (GEO, destdir, AnnotGPL=FALSE) {
  GEO <- toupper(GEO)
  con = getCurlHandle( ftp.use.epsv = FALSE)
  a <- getURL(sprintf("ftp://ftp.ncbi.nih.gov/pub/geo/DATA/SeriesMatrix/%s/", 
                      GEO), curl=con, verbose=TRUE)
  tmpcon <- textConnection(a, "r")
  b <- read.table(tmpcon)
  close(tmpcon)
  b <- as.character(b[, ncol(b)])
  message(sprintf("Found %d file(s)", length(b)))
  ret <- list()
  for (i in 1:length(b)) {
    message(b[i])
    destfile = file.path(destdir, b[i])
    if (file.exists(destfile)) {
      message(sprintf("Using locally cached version: %s", 
                      destfile))
    }
    else {
      download.file(sprintf("ftp://ftp.ncbi.nih.gov/pub/geo/DATA/SeriesMatrix/%s/%s", 
                            GEO, b[i]), destfile = destfile, mode = "wb", method="wget", quiet=FALSE)
    }
    ret[[b[i]]] <- GEOquery:::parseGSEMatrix(destfile)$eset
  }
  return(ret)
}

getGEOfile <- function (GEO, destdir = tempdir(), AnnotGPL = FALSE,
                        amount = c("full", "brief", "quick", "data")) {
  amount <- match.arg(amount)
  geotype <- toupper(substr(GEO, 1, 3))
  mode <- "wb"
  if (geotype == "GDS") {
    gdsurl <- "ftp://ftp.ncbi.nih.gov/pub/geo/DATA/SOFT/GDS/"
    myurl <- paste(gdsurl, GEO, ".soft.gz", sep = "")
    destfile <- file.path(destdir, paste(GEO, ".soft.gz", 
                                         sep = ""))
  }
  if (geotype == "GSE" & amount == "full") {
    gseurl <- "ftp://ftp.ncbi.nih.gov/pub/geo/DATA/SOFT/by_series/"
    myurl <- paste(gseurl, GEO, "/", GEO, "_family.soft.gz", 
                   sep = "")
    destfile <- file.path(destdir, paste(GEO, ".soft.gz", 
                                         sep = ""))
  }
  if (geotype == "GSE" & amount != "full" & amount != "table") {
    gseurl <- "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi"
    myurl <- paste(gseurl, "?targ=self&acc=", GEO, "&form=text&view=", 
                   amount, sep = "")
    destfile <- file.path(destdir, paste(GEO, ".soft", sep = ""))
    mode <- "w"
  }
  if (geotype == "GPL") {
    if (AnnotGPL) {
      gdsurl <- "ftp://ftp.ncbi.nih.gov/pub/geo/DATA/annotation/platforms/"
      myurl <- paste(gdsurl, GEO, ".annot.gz", sep = "")
      destfile <- file.path(destdir, paste(GEO, ".annot.gz", 
                                           sep = ""))
    }
    else {
      gseurl <- "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi"
      myurl <- paste(gseurl, "?targ=self&acc=", GEO, "&form=text&view=", 
                     amount, sep = "")
      destfile <- file.path(destdir, paste(GEO, ".soft", 
                                           sep = ""))
      mode <- "w"
    }
  }
  if (geotype == "GSM") {
    gseurl <- "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi"
    myurl <- paste(gseurl, "?targ=self&acc=", GEO, "&form=text&view=", 
                   amount, sep = "")
    destfile <- file.path(destdir, paste(GEO, ".soft", sep = ""))
    mode <- "w"
  }
  if (!file.exists(destfile)) {
    download.file(myurl, destfile, mode = mode, quiet = FALSE, method="wget")
    message("File stored at: ")
    message(destfile)
  }
  else {
    message(sprintf("Using locally cached version of %s found here:\n%s ", 
                    GEO, destfile))
  }
  invisible(destfile)
}
assignInNamespace("getAndParseGSEMatrices", getAndParseGSEMatrices, "GEOquery")
assignInNamespace("getGEOfile", getGEOfile , "GEOquery")
