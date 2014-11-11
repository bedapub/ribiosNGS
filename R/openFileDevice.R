getDefaultFontFamily <- function() {
  psFonts <- names(postscriptFonts())
  if("ArialMT" %in% psFonts) {
    return("ArialMT")
  } else if ("Helvetica" %in% psFonts()) {
    return("Helvetica")
  } else {
    return("")
  }
}
openFileDevice <- function(filename, width=7, height=7, dpi=300L, family) {
  filetype <- extname(filename)
  if(identical(filetype,"pdf")) {
    pdf(filename, width=width, height=height, useDingbats=FALSE, family=family)
  } else if (identical(filetype, "png")) {
    png(filename, width=width, height=height, units="in", type="cairo", res=dpi) ## family option not applicable since it can not be missing as in the case of PDF
  } else if(identical(filetype,"tiff") || identical(filetype, "tif")) {
    tiff(filename, width=width, height=height, units="in", res=dpi)
  } else if(identical(filetype, "bmp")) {
    bmp(filename, width=width, height=height, units="in", res=dpi)
  } else if(identical(filetype, "jpeg") || identical(filetype, "jpg")) {
    jpeg(filename, width=width, height=height, units="in", res=dpi, quality=90)
  } else {
    warning("Unrecognized output format '", filetype, "'. PDF will be used")
    pdf(filename, width=width, height=height, useDingbats=FALSE, family=family)
  }
}

closeFileDevice <- function() {
  invisible(dev.off())
}
