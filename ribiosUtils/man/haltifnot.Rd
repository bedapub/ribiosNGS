\name{haltifnot}
\alias{haltifnot}
\title{
  Ensure the Truth of R Expressions and Print Defined Error Message if NOT
}
\description{
  If any of the expressions in \sQuote{...} are not \emph{all} \emph{TRUE}, \emph{stop} is
  called, producing an error message defined by the \emph{msg} parameter.
}
\usage{
haltifnot(..., msg = "Error undefined. Please contact the developer")
}
\arguments{
  \item{\dots}{any number of \sQuote{logical} R expressions, which
    should evaluate to \code{TRUE}}
  \item{msg}{Error message.}
}
\details{
  The function is adapted from the \code{stopifnot} function, with the
  difference that the error message can be defined the programmer. With
  \code{haltifnot} error message can be more informative, which is
  desired for diagnostic and user-interation purposes.
}
\value{
  \code{NULL} if all statements in \code{...} are \code{TRUE}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{stop}}, \code{\link{warning}} and \code{\link{stopifnot}}
}
\examples{
haltifnot(1==1, all.equal(pi, 3.14159265), 1<2) ## all TRUE
m <- matrix(c(1,3,3,1), 2,2)
haltifnot(m == t(m), diag(m) == rep(1,2)) ## all TRUE

op <- options(error = expression(NULL))
# "disable stop(.)"  << Use with CARE! >>
     
haltifnot(all.equal(pi, 3.141593),  2 < 2, all(1:10 < 12), "a" < "b",
          msg="not all conditions are TRUE. Please contact the devleoper")
options(op)# revert to previous error handler
}
