#' Get default font family
#' @return Character string, the default font family
#' @importFrom grDevices postscriptFonts
#' @export getDefaultFontFamily
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




#' Open a device as a file preparing for plotting in the file
#' 
#' The function \code{openFileDevice} opens a device of the type specified by
#' the file extension name. It such prepares the file for visualizing data.
#' User must call \code{dev.off} once the writing (plotting) to the device is
#' finished.
#' 
#' \code{closeFileDevice} quietly closes the current device: it does not print
#' the information of the next device.
#' 
#' The function \code{openFileDevice} calls \code{extname} to determine the
#' file type to be drawn in. Currently supported types include \code{PDF},
#' \code{tiff} (\code{tif}), \code{bmp}, \code{jpeg} (\code{jpeg}). When the
#' file type is not recognized, the \code{PDF} format is used as a fallback.
#' 
#' As an example, \code{myplot.pdf} will triggers openning a PDF device,
#' \code{newplot.png} a PNG device, and \code{oldplot.tiff} a TIFF device,
#' whereas \code{myfile.abc} will be openned as a PDF device.
#' 
#' For bitmap files like \code{BMP}, \code{JPEG},\code{PNG} and \code{TIFF}, we
#' use \code{inch} as the size unit in order to be compatible with PDF. And the
#' resolution is always set to 300dpi.Furthermore, JPEG quality is set to 90
#' instead of the default value 75, and TIFF do not use any compression. These
#' settings follow our practices for scientific publication while allowing
#' generic post-precessing of figures.
#' 
#' @aliases openFileDevice closeFileDevice
#' @param filename Character, file name to be written to. The type of file is
#' determined by the extension. See details below.
#' @param width Number, figure width of the file in \emph{inch}.
#' @param height Number, figure height of the file in \emph{inch}.
#' @param dpi Number, resolution as \dQuote{dots per inch}. For publication
#' 300dpi is usually enough.
#' @param family Font family name. Only applicable to PDF files
#' @return Both functions are used for its side effect.
#' @note After plotting, user should call \code{dev.off} to close the device in
#' the file, otherwise the file can probably not be read.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{extname}} for getting extension name of file. See
#' \code{\link{pdf}}, \code{\link{png}}, \code{\link{jpeg}}, \code{\link{tiff}}
#' and \code{\link{bmp}} for file formats.
#' @examples
#' 
#' if(interactive()) {
#'   tempfile1 <- paste(tempfile(), ".pdf", sep="")
#'   openFileDevice(tempfile1)
#'   plot(rnorm(100), rnorm(100))
#'   closeFileDevice()
#' 
#'   tempfile2 <- paste(tempfile(), ".png", sep="")
#'   openFileDevice(tempfile2, width=5, height=5)
#'   plot(rnorm(100), rnorm(100))
#'   closeFileDevice()
#' }
#' 
#' @importFrom grDevices pdf png tiff bmp jpeg
#' @export openFileDevice
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

#' @importFrom grDevices dev.off
#' @export closeFileDevice
closeFileDevice <- function() {
  invisible(dev.off())
}
