\name{empval}
\alias{empval}
\title{Get empirical p-value}
\description{
  Calculate empirical p-values from real values (\code{a}) and simulated
  values (\code{b}).
}
\usage{
empval(stat, sim)
}
\arguments{
  \item{stat}{A numeric vector of calculated statistic from the actual data}
  \item{sim}{A numeric vector (or matrix) of simulated statistics,
    e.g. by Monte-Carlo methods}
}
\details{
  The estimate of the P-value is obtained as \eqn{\hat{p}=(r+1)/(n+1)},
  where \code{n} is the number of replicate samples that have been
  simulated and \code{r} is the number of these replicates that produce
  a test statistic greater than or equal to that calculated for the
  actual data.
}
\value{
  A vector of empirical p values (of the same length as \code{a}).
}
\references{
  Davison AC, Hinkley DV (1997) Bootstrap methods and their
  applications. Cambridge University Press, Cambridge, United Kindom.

  North BV, Curtis D, Sham PC (2002) A note on the calculation of
  empirical p values from Monte Carlo Procedures. Am J Hum Genet. 2002
  August; 71(2):439--441.
}
\author{
  Jitao David Zhang
}

\examples{
  set.seed(1995)
  testStat <- c(-100, -3, -1, 0, 1, 3, 100)
  testSim <- rnorm(1000)
  empval(stat=testStat, sim=testSim)
}