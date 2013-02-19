\name{refactor}
\alias{refactor}
\title{
  Re-order factor levels by a named character vector
}
\description{
  Re-order factor levels by a named character vector
}
\usage{
refactor(factor, levels)
}
\arguments{
  \item{factor}{A vector of factors}
  \item{levels}{A vector of named strings. Names are old factor levels,
    and values are new factor levels.}
}
\details{
  \code{levels} must include all levels, otherwise the function prints
  error message and stops for the safety of replacing factor levels.

  Note that the order of \code{levels} matters. It determines the order
  of levels.
}
\value{
  A vector of factors having  new levels.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
\code{\link{relevels}}, which does not require all levels to be replaced.
}
\examples{
teams <- factor(c("HSV", "FCB", "HSV", "BVB", "S04"),levels=c("HSV",
"BVB", "S04", "FCB"))
new.levels <- c("HSV"="Hamburger SV", "S04"="Schalke 04", "FCB"="FC Bayern", "BVB"="Borussia Dortmund")
new.teams <- refactor(teams, new.levels)

levels(teams)
levels(new.teams)
}

