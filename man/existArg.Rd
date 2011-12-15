\name{existArg}
\alias{existArg}
\title{
  Argument existing or not
}
\description{
  Test an argument option exist or not in the command line
}
\usage{
existArg(args)
}
\arguments{
  \item{args}{One or more option names, without leading minus sign}
}
\details{
  Options are those arguments with a leading minus sign
  (e.g. "-opt"). This function tells whether queried options exist in
  the argument list.
}
\value{
  A logical vector of the same length as the input query vector
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\seealso{
  \code{\link{getArg}} to get argument values.
}
\examples{
comm <- paste(c("Rscript --vanilla -e", "'", "library(ribiosArg);",
                "existArg(c(\"opt\", \"opt2\", \"opt3\"))", "'",
                 "-opt abc -opt3"), collapse=" ")
system(comm)
}

