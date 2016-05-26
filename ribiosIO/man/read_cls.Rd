\name{read_cls}
\alias{read_cls}
\title{
  Read cls files
}
\description{
  \code{readCls} reads CLS format files, returning a vector of
  factors, the levels of which encoding sample groups.
}
\usage{
read_cls(cls.file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cls.file}{The name of the cls file }
}
\details{
  See example below.
}
\value{
  A vector of factors, the levels of which encoding sample group.s
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{read_gct_matrix}}
}
\examples{
idir <- system.file("extdata", package="ribiosIO")

sample.cls <- read_cls(file.path(idir, "test.cls"))
}