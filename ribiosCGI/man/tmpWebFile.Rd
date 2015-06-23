\name{tmpWebFile}
\alias{tmpWebFile}
\alias{tmpWebURL}
\alias{RIBIOS_CGI_TMP_DIR}
\alias{RIBIOS_CGI_TMP_URL}
\title{Temporary file in a URL-accessible directory}
\description{
  When handling web requests, it is sometimes necessary to write
  temporary files to the server file system, which is accessible by an
  URL. \code{tmpWebFile} returns a temporary file name. The URL can be
  extracted from the temp file name by \code{tmpWebURL}.

  The default temporary directory and its URL address has been adjusted to the
  BIOS installation: \code{RIBIOS_CGI_TMP_DIR} is the temporary
  directory, and \code{RIBIOS_CGI_TMP_URL} points to it. It is up to
  users to make sure that the directory and the URL points to the same
  place in case the values are explicitly specified.
}
\usage{
tmpWebFile(pattern = "file", fileext = "", tmpdir=RIBIOS_CGI_TMP_DIR)
tmpWebURL(tmpFile, tmpurl=RIBIOS_CGI_TMP_URL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{Initial part of the temporary file name}
  \item{fileext}{File name extesion, i.e. \dQuote{.png} (note the dot)}
  \item{tmpdir}{Temporary directory}
  \item{tmpFile}{Temporary file name returned by \code{tmpWebFile}}
  \item{tmpurl}{URL pointing to the temporary directory}
}
\value{
  \code{tmpWebFile} returns the full name of the temporary file, and
  \code{tmpWebURL} returns the valid URL.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
tmpFile <- tmpWebFile(pattern="file", fileext=".png")
print(tmpFile)
print(tmpWebURL(tmpFile))
}
