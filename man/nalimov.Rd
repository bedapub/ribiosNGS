\name{nalimov}
\alias{nalimov}
\title{
  Outlier detection with Nalimov test
}
\description{
  The \code{nalimov} function performs outlier detection with the
  Nalimov test. 
}
\usage{
nalimov(x, sig = c("0.95", "0.99", "0.995"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A numerical vector}
  \item{sig}{Character string indicating the significance level
    (1-alpha). Available values include \dQuote{0.95}, \dQuote{0.99} and
    \dQuote{0.995}.}
}
\details{
  The function performs the Nalimov test in a non-recursive
  fashion.
}
\value{
  A numerical vector removing outliers detected by the Nalimov test.
}
\references{
   Robust statistics, Frank R. Hampel, Elvezio M. Ronchetti, Peter
   J. Rousseuw, Werner A. Stahel, John Wiley & Sons 1986-
}
\author{
  Jitao David Zhang, Martin Ebeling and Clemens Broger
}
\examples{
x <- c(30.41,30.05,30.49,29.22,30.40,30.42)
nalimov(x, sig="0.95")
}

