\name{refactorNum}
\alias{refactorNum}
\title{
  Sort numeric factor levels by values
}
\description{
  Factor variables with numbers as levels are alphabetically ordered by
  default, which requires rearrangements for various purposes,
  e.g. modelling or visualizations. This function re-orders levels of
  numeric factor variables numerically.
}
\usage{
refactorNum(x, decreasing = FALSE)
}
\arguments{
  \item{x}{A factor variable with numeric values as levels}
  \item{decreasing}{Logical, should the levels sorted descendingly?}
}
\value{
  A factor variable, with sorted numeric values as levels
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
(nums <- factor(c("2","4","24","1","2","125","1","2","125")))
(nums.new <- refactorNum(nums))
}

