\name{cgiSendFile}
\alias{cgiSendFile}
\title{
  Send a file from server to the client
}
\description{
  Send a file from server to the client in its format, such as PDF or PNG.
}
\usage{
cgiSendFile(file, header = NULL)
}
\arguments{
  \item{file}{File name on the server side}
  \item{header}{CGI header. If not NULL, it will be converted to
    character string. It should match the file type, for instance
    \code{application/pdf} for PDF files, and \code{image/png} for PNG files.}
}
\details{
  The function can be used to send a file from the server side to the
  client side.
}
\value{
  Side effect is used.
}
\references{
  See \code{/apps/bi/apps/arcadeviewer/arcadereport.c} as an example of
  using \code{fwrite} to write binary contents to \code{stdout}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  The function deliberately breaks the rule of R to write the file
  content to stdout from the compiled code. It is only purposed to be
  used as a CGI server side function.
}
\seealso{
  \code{\link{cgiInit}} to initialize a CGI program,
  \code{\link{cgiGet2Post}} to convert \code{GET} options to \code{POST}
  strings, and \code{\link{cgiHeader}} to set CGI header.
}
\examples{
\dontrun{
cgiInit()
cgiGet2Post()
cgiSendFile("test.png", "image/png")

## the last command is equivalent to the following two commands:
## cgiHeader("image/png")
## cgiSendFile("test.png")

}
}

