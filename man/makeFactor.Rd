\name{makeFactor}
\alias{makeFactor}
\alias{parseFactor}
\title{
  Parse factors from command line
}
\description{
  Parse factors from command line with \code{parseFactor}. The auxillary
  function \code{makeFactors} can be used to construct a vector of
  factors with groups and levels.
}
\usage{
makeFactor(groups, levels=NULL, make.names = TRUE, verbose=FALSE)
parseFactor(rgroups, rlevels=NULL, make.names = TRUE, collapse=",")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groups}{A character vector specifying strings of factor}
  \item{levels}{A character vector specifying levels of factor}
  \item{make.names}{Logical, force making syntactically valid names in
    R}
  \item{rgroups}{Command-line input raw group string}
  \item{rlevels}{Command-line input levels, can be missing or NULL.}
  \item{collapse}{Collapse character used in command line}
  \item{verbose}{Logical, whether renaming of syntactically invalid contrast names should be warned.}
}
\details{
  \code{makeFactor} checks whether all unique values of the
  \code{groups} parameter exist in \code{level}. If not, it stops the
  program and reports the error; otherwise, it builds a factor using the
  specified level. If \code{make.names} is \code{TRUE}, levels are
  checked for syntactic validity in R in order to make design/contrast
  matrix construction easier.

  If \code{levels} is unspecified or \code{NULL}, it will be set as the
  existing levels if \code{groups} is a factor, otherwise the alphabetical
  order levels (which may vary depending on the locale).

  \code{parseFactor} is a wrapper of \code{makeFactor} that accepts raw
  command-line inputs to build factors.
}
\value{
  A vector of factors
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\note{
  It is possible to have levels that are unobserved in groups, but not
  other way around. See examples below (which are not run by default).
}
\seealso{
  \code{\link{factor}}
}
\examples{
rgroup <- c("A,B,C,D,B,C,A,D,B")
rlevels <- c("D,A,B,C")
parseFactor(rgroup, rlevels)

groups <- c("ATest", "Control", "Control", "ATest")
levels <- c("Control", "ATest")
makeFactor(groups, levels)

## if 'groups' is a factor and 'levels' NULL or missing, its levels are respected
groups <- factor(c("B", "C", "A", "D"), levels=c("D","C","A","B"))
makeFactor(groups)

\dontrun{
groups <- c("ATest", "Control", "Control", "ATest")
levels <- c("Control", "ATest", "Unknown")
makeFactor(groups, levels)

groups <- c("ATest", "Control", "Control", "ATest", "BTest")
levels <- c("Control", "ATest")
makeFactor(groups, levels)
}
}

