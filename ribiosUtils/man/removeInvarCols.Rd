\name{removeInvarCol}
\alias{removeInvarCol}
\alias{isInvarCol}
\alias{isVarCol}
\title{
  Remove invariable columns from a data frame or matrix
}
\description{
  Columns with one unique value are invariable. The functions help to
  remove such columns from a data frame (or matrix) in order to
  highlight the variables. 
}
\usage{
isVarCol(df)
isInvarCol(df)
removeInvarCol(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{A data frame or matrix}
}
\details{
  \code{removeInvarCol} the data frame removing invariable column(s).

  \code{isVarCol} and \code{isInvarCol} are helper functions, returning a logical vector indicating the variable and invariable columns respectively.
}
\value{
  \code{isVarCol} and \code{isInvarCol} return a logical vector
  indicating the variable and invariable columns respectively.

  \code{removeInvarCol} removes invariable columns.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
 testDf <- data.frame(a=1:4, b=7, c=LETTERS[1:4])
isVarCol(testDf)
isInvarCol(testDf)
removeInvarCol(testDf)
}
