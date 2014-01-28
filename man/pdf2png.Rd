\name{pdf2png}
\alias{pdf2png}
\title{
  Use 'convert' (ImageMagick) to convert PDF to high-quality PNG
}
\description{
  The function makes a system call to convert PDF files to high-quality (300
  dpi) PNG files.
}
\usage{
pdf2png(..., convert = "convert", density = 300, outdir=NULL, wait = FALSE)
}
\arguments{
  \item{\dots}{PDF files}
  \item{convert}{Name of the convert program}
  \item{density}{DPI. Default 300 is good enough for publications in
    most biology/medicine journals}
  \item{outdir}{Output directory. If the value is \code{NULL}, the
    output files will be written in the same directory as the input file}
  \item{wait}{Logical, should the function wait until the conversion is finished?}
}
\value{
  PNG file names are returned invisibly.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
  tmpdir <- tempdir()

  pdffile <- file.path(tmpdir, "test-plot.pdf")
  pdf(pdffile)
  plot(1:5)
  dev.off()

  pdf2png(pdffile)
  
  testfile <- system.file("/doc/intro.pdf", package="limma")
  if(file.exists(testfile)) {
    pdf2png(testfile, outdir=tmpdir)
    Sys.sleep(1)
    dir(tmpdir)

   ## or waiting
    pdf2png(testfile, outdir=tmpdir, wait=TRUE)
  }
}
