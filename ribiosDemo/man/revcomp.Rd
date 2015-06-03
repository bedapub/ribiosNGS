\name{revcomp}
\alias{revcomp}
\alias{revcompNative}
\alias{makeSeq}
\title{
  Get Reverse-Complementary DNA/RNA Sequence
}
\description{
  The \code{revcomp} function demonstrates using the \code{revcomp}
  routine in the BIOS system to get the reverse-complementary sequence
  of a given DNA/RNA sequence.

  In parallel we provide an native R implementation with almost the same
  functionality. By benchmarking we could show that the
  BIOS-implementation is about 10 times faster for long sequences
  (10Mb). Considering that the BIOS implementation performs extra checks
  for non-DNA/RNA bases, we can safely conclude that BIOS offers
  advantages in handling characters and therefore in sequence analysis
  tasks.

  \code{makeSeq} is a helper function to generate random DNA/RNA
  sequences.
}
\usage{
revcomp(x)
revcompNative(x)
makeSeq(bases=c("A","T","G","C"), len=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Character vector, DNA or RNA sequences}
  \item{bases}{Character vector, bases used for building sequences}
  \item{len}{Integer, sequence length}
}
\details{
  The \code{revcomp} function calls the C routine \code{bios_revcomp},
  which in turn performs a C-routine named \code{revcomp} in the BIOS
  system. The C-code demonstrates how to build a \code{Stringa} object
  and to operate on the object.

  We do not claim that our native R implementation is the most effective
  one, despite of the fact that it might be one of the most
  straight-forward and easiest approach. The benchmark however gives a
  intuitive comparison of performces on character handling between R and C.
}
\value{
  A character vector of the same length as input, with reversed and
  complementary DNA/RNA sequences.
}
\references{
  BIOS documentation
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, with input from
  Clemens Broger
}
\examples{
revcomp("ATGC")
revcomp(c("ATGC", "CGTGCA"))

revcomp(c("AUGUGUUAGUC", "UUAUUCUUN"))
revcompNative(c("AUGUGUUAGUC", "UUAUUCUUA"))

testStr <- sapply(1:5, function(t) makeSeq(len=50000))
system.time(testRc <- revcomp(testStr))
system.time(testRcNat <- revcompNative(testStr))
stopifnot(identical(testRc, testRcNat))
}
