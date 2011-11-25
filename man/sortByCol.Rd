\name{sortByCol}
\alias{sortByCol}
\title{Sort data.frame rows by values in specified columns}
\description{
  Sort rows of an \code{data.frame} by values in specified columns.
}
\usage{
sortByCol(data.frame, columns, na.last=TRUE, decreasing = TRUE, orderAsAttr=FALSE)
}
\arguments{
  \item{data.frame}{A \code{data.frame} object}
  \item{columns}{Column name(s) which sould be ordered}
  \item{na.last}{Logical, whether NA should be sorted as last}
  \item{decreasing}{Logical, whether the sorting should be in the
    decreasing order}
  \item{orderAsAttr}{Logical, whether the order index vectors should be
    returned in the attribute \dQuote{order} of the sorted \code{data.frame}}
}
\details{
  Columns can be specified by integer indices, logical vectors or
  character names.
}
\value{
  Sorted \code{data.frame}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
sample.df <- data.frame(teams=c("HSV", "BVB", "FCB", "FCN"),pts=c(18,17,17,9), number=c(7,7,6,6))
sortByCol(sample.df, 1L)
sortByCol(sample.df, 1L, decreasing=FALSE)

sortByCol(sample.df, c(3L, 1L))
sortByCol(sample.df, c(3L, 1L), decreasing=FALSE)
sortByCol(sample.df, c(3L, 2L))

sortByCol(sample.df, c(TRUE, FALSE, TRUE))

sortByCol(sample.df, c("teams", "pts"))
sortByCol(sample.df, c("pts", "number", "teams"))
sortByCol(sample.df, c("pts", "teams", "number"))
}
