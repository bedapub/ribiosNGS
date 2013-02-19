\name{isError}
\alias{isError}
\title{Tell whether an object is an error}
\description{
  Determines whether an object is of class \code{try-error}
}
\usage{
isError(x)
}
\arguments{
  \item{x}{Any object, potentially produced within a \code{try-error}
    structure.}
}

\value{
  Logical value, \code{TRUE} if \code{x} inherits the \code{try-error} class.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
if(exists("nonExistObj")) rm(nonExistsObj)
myObj <- try(nonExistObj/5, silent=TRUE)
isError(myObj)
}

