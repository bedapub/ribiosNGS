\name{openFileDevice}
\alias{openFileDevice}
\alias{closeFileDevice}
\title{
  Open a device as a file preparing for plotting in the file
}
\description{
  The function \code{openFileDevice} opens a device of the type specified by the file
  extension name. It such prepares the file for visualizing data. User
  must call \code{dev.off} once the writing (plotting) to the device is
  finished.

  \code{closeFileDevice} quietly closes the current device: it does not
  print the information of the next device.
}
\usage{
openFileDevice(filename, width = 7, height = 7, dpi=300L, family)
closeFileDevice()
}
\arguments{
  \item{filename}{Character, file name to be written to. The type of
    file is determined by the extension. See details below.}
  \item{width}{Number, figure width of the file in \emph{inch}.}
  \item{height}{Number, figure height of the file in \emph{inch}.}
  \item{dpi}{Number, resolution as \dQuote{dots per inch}. For
    publication 300dpi is usually enough.}
  \item{family}{Font family name. Only applicable to PDF files}
}
\details{
  The function \code{openFileDevice} calls \code{extname} to determine the file type to be
  drawn in. Currently supported types include \code{PDF}, \code{tiff}
  (\code{tif}), \code{bmp}, \code{jpeg} (\code{jpeg}). When the file
  type is not recognized, the \code{PDF} format is used as a fallback.

  As an example, \code{myplot.pdf} will triggers openning a PDF device,
  \code{newplot.png} a PNG device, and \code{oldplot.tiff} a TIFF
  device, whereas \code{myfile.abc} will be openned as a PDF device.

  For bitmap files like \code{BMP}, \code{JPEG},\code{PNG} and
  \code{TIFF}, we use \code{inch} as the size unit in order to be
  compatible with PDF. And the resolution is always set to
  300dpi.Furthermore, JPEG quality is set to 90 instead of the default
  value 75, and TIFF do not use any compression. These settings follow our
  practices for scientific publication while allowing generic
  post-precessing of figures.
}
\value{
  Both functions are used for its side effect.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  After plotting, user should call \code{dev.off} to close the device in
  the file, otherwise the file can probably not be read.
}

\seealso{
  \code{\link{extname}} for getting extension name of file. See
  \code{\link{pdf}}, \code{\link{png}},  \code{\link{jpeg}},
  \code{\link{tiff}} and \code{\link{bmp}} for file formats.
}
\examples{
if(interactive()) {
  tempfile1 <- paste(tempfile(), ".pdf", sep="")
  openFileDevice(tempfile1)
  plot(rnorm(100), rnorm(100))
  closeFileDevice()

  tempfile2 <- paste(tempfile(), ".png", sep="")
  openFileDevice(tempfile2, width=5, height=5)
  plot(rnorm(100), rnorm(100))
  closeFileDevice()
}
}

