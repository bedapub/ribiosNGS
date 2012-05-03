\name{queryUrl}
\alias{queryUrl}
\title{
  Query URL with bioinfo cookie
}
\description{
  Query URL with bioinfo cookie. The low-level function is mainly used
  for debugging and advanced uses of server-client data
  transmission. End users are more likely to use other functions which
  call \code{queryUrl} internally.
}
\usage{
queryUrl(url)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{A URL address, most probably within Roche Bioinfo
    enrivonment or UDIS}
}
\details{
  To query a URL in Roche bioinfo environment, user must authenticate
  him/herself by user name. Successful authentication returns a valid
  cookie for requesting data or actions from the server side. This step
  is taken over by the \code{bioinfoCookie} function internally. Once
  the cookie is ready, the \code{postUrl} function queries the given URL
  with the cookie appended in the HTTP header, in a \code{POST} fashion.
}
\value{
  Returned information from the server, as strings
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  Currently the function uses \code{\link{postForm}} function from the \code{RCurl}
  function. It is possible that in the future the function will be
  solely dependent on BIOS.
}
\seealso{
  \code{\link{bioinfoCookie}} for getting cookies from Bioinfo
  server, \code{\link{postForm}} for posting forms
}
\examples{
## get Gene information from GTI
QURL <- "http://bioinfo.bas.roche.com:8080/bicgi/gti.cgi?command=getcontent&geneid=1432&sections=gene&format=xml"
myGeneXML <- queryUrl(QURL)
cat(myGeneXML)
}