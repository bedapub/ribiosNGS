\name{idev}
\alias{idev}
\alias{ipdf}
\title{
  Interative dev.print and pdf print
}
\description{
  Execute dev.print only if R session is interactive.

  \code{ipdf} is a shortcut in case PDF is used as the device, with the
  twist that \code{useDingbats} is set to \code{FALSE} by default. See NOTE.
}
\usage{
idev(...)
ipdf(file,...)
}
\arguments{
  \item{\dots}{Parameters passed to \code{\link{dev.print}}}
  \item{file}{PDF file name}
}
\details{
  \code{dev.print} will make a R-script fail if the session is not
  interactive (e.g. when the script is excuted with the \code{-f} option
  from \code{R} command line). Function \code{idev} checks first whether
  the session is interative, and executes \code{dev.print} only if the
  session is interactive.

  A commonly used shortcut is \code{ipdf}, which prints the current
  device to a PDF file.
}
\value{
  Side effect used.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  \code{useDingbats} is set to \code{FALSE} in \code{ipdf}. Setting the
  option to \code{TRUE} causes problem in importing the PDF to Inkscape,
  a vector-based figure modifying software. Though the option may reduces
  smaller and (according to the R manual) better output, we have noticed no difference. 
}

\seealso{
  \code{\link{dev.print}}, \code{\link{pdf}}
}
\examples{
tmfile <- tempfile()
plot(1:15, type="h")
idev(png, tmfile,width=600, height=800)
ipdf(tmfile)
}