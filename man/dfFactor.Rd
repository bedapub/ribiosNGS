\name{dfFactor}
\alias{dfFactor}
\title{Get a factor vector for a data.frame}
\description{
  The function try to assign a factor vector for a \code{data.frame}
  object. See details below.
}
\usage{
dfFactor(df, sample.group)
}
\arguments{
  \item{df}{A \code{data.frame}}
  \item{sample.group}{A character, number or a vector of factors, from
    which the factor vector should be deciphered. See details below.}
}
\details{
  The function tries to get a factor vector of the same length as the
  number of rows in the \code{data.frame}. The determination is done in
  the following order:
  Step 1: It tries to find a column in the \code{data.frame} with
    the name as given by \code{sample.group}. If found, this column is
    transformed into a factor if not and returned.
  Step 2: It tries to interpret the \code{sample.group} as an
    integer, as the index of the column in the \code{data.frame} giving
    the factor.
  Step 3: When \code{sample.group} itself is a vector of
    the same length as the data.frame, it is cast to factor when it is
    still not and returned.
    
  Otherwise the program stops with error.
  
}
\value{
  A factor vector with the same length as the \code{data.frame}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
df <- data.frame(gender=c("M", "M", "F", "F", "M"),
age=c(12,12,14,12,14), score=c("A", "B-", "C", "B-", "A"))
dfFactor(df, "gender")
dfFactor(df, "score")
dfFactor(df, 1L)
dfFactor(df, 2L)
dfFactor(df, df$score)
\dontrun{dfFactor(df, "mark")}
}
