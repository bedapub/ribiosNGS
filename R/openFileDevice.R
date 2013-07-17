openFileDevice <- function(filename, width=7, height=7) {
  RES_INCH <- 300L
  filetype <- extname(filename)
  if(identical(filetype,"pdf")) {
    pdf(filename, width=width, height=height)
  } else if (identical(filetype, "png")) {
    png(filename, width=width, height=height, units="in", type="cairo", res=RES_INCH)
  } else if(identical(filetype,"tiff") || identical(filetype, "tif")) {
    tiff(filename, width=width, height=height, units="in", res=RES_INCH)
  } else if(identical(filetype, "bmp")) {
    bmp(filename, width=width, height=height, units="in", res=RES_INCH)
  } else if(identical(filetype, "jpeg") || identical(filetype, "jpg")) {
    jpeg(filename, width=width, height=height, units="in", res=RES_INCH, quality=90)
  } else {
    warning("Unrecognized output format. PDF will be used")
    pdf(filename, width=width, height=height)
  }
}
