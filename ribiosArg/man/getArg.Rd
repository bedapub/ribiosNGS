\name{getArg}
\alias{getArg}
\title{
  Get argument value from command line
}
\description{
  Get the value(s) of an argument option from command line.
}
\usage{
getArg(args, onlyArg = FALSE, missingArg = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{args}{Option names, without the leading minus sign}
  \item{onlyArg}{What value should be returned if only the option is
    available and no value has been provided}
  \item{missingArg}{What value should be returned if the option is not available}
}
\details{
  Options are those arguments with a leading minus sign. They can
  have one or more values following them, which will be taken as the value
  of the option. If no such values are availble, user could decide how to
  interpret the option by setting the \code{onlyArg} parameter. Similarly,
  missing options can be handled by \code{missingArg}

  From version 1.0.3 \code{onlyArg} and \code{missingArg} accepts
  \code{NULL} as inputs.
}
\value{A list when more than one option were queried; or a vector if only
  one option was queried.}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\seealso{
  \code{\link{existArg}} to test whether option exist.
}
\examples{
comm <- paste(c("Rscript --vanilla -e", "'", "library(ribiosArg);",
                "getArg(c(\"opt\", \"opt3\", \"opt2\", \"opt4\", \"5way\"), onlyArg=FALSE, missingArg=\"missing\")", "'",
                 "-opt abc -opt3 -opt4 -3,5 -5way true"), collapse=" ")
system(comm)
}
