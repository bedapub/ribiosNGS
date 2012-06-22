\name{ChipFetcher2ExpressionSet}
\alias{ChipFetcher2ExpressionSet}
\title{
  Import ChipFetcher export files into an \code{ExpressionSet} object.
}
\description{
  Import files exported by Roche web-tool \code{ChipFetcher} into an
  \code{ExpressionSet} object.
}
\usage{
ChipFetcher2ExpressionSet(filename, doAnnotation=TRUE, chip, orthologue = FALSE)
}
\arguments{
  \item{filename}{Exported file name of ChipFetcher}
  \item{doAnnotation}{Logical, whether features should be
    annotated. \code{TRUE} by default}
  \item{chip}{Chip type used to annotate the features,
    e.g. \code{HG-U133_PLUS_2}. In case missing, chips are automatically
    mapped. Assigning the value of \code{chip} accelerates the probe
    mapping step.}
  \item{orthologue}{Logical, whether features should be mapped to human
    orthologues? By default \code{FALSE}.}
}
\details{
  \code{chip} and \code{orthologue} are only valid when
  \code{doAnnotaiton} is set to \code{TRUE}.
}
\value{
  An \code{ExpressionSet} object.
}
\references{
  \url{http://bioinfo.bas.roche.com:8080/bicgi/chipfetcher}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
%%\examples{

%%}
