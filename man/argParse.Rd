\name{argParse}
\alias{argParse}
\alias{argGet}
\alias{argGetPos}
\alias{argPresent}
\title{
  A BIOS-style parser of command-line parameters
}
\description{
  We provide a BIOS-style command-line parser, which checks, parses and
  returns command-line parameters by the convention used in BIOS.
}
\usage{
argParse(optargs, reqargs, usage = paste(scriptName(), "-h"))
argGet(opt)
argGetPos(opt, ind=1L)
argPresent(opt)
}
\arguments{
  \item{optargs}{String describing optional arguments. Syntax:
    \code{<optname1>[,paramcnt1] <optname2>[,paramcnt2]\dots}. Example:
    \dQuote{verbose outfile,1} means the command line has the syntax
    \code{prog [-verbose] [outfile name]}. It can be an empty string to
    express \dQuote{no options}. The value for \code{paramcnt} is 0.}
  \item{reqargs}{String describining required arguments. Syntax:
    \code{<argname1> <argname2>\dots}. Example: \dQuote{infile outfile}
    means the command line has the syntax \code{prog [-infile ]infile
      [-outfile ]coutfile}. Even if it is empty, it is checked that at
    least one non-optional value is given.}
  \item{usage}{A character string to be printed if the command-line
    option parsing fails}
  \item{opt}{Name of a command line option}
  \item{ind}{Index of the value to be fetched, see below.}
}
\details{
  \code{argParse} must be called before \code{argGet},\code{argGetPos}
  or \code{argPresent}. It checks whether the command line syntax agrees
  with the specification of \code{optargs} and \code{reqargs}. If not,
  the \code{usage} message is printed and the program exists.

  If the syntax was found correct, \code{argGetPos} can be called to
  fetch the \code{ind}th value of the option \code{opt} (indexing from 1). For instance,
  if the following option \code{-ranges 3 5} is defined,
  argGetPos(\dQuote{range}, 2) returns \code{5}. \code{argGet} is
  a shortcut to fetch the first element. And \code{argPresent} returns a
  boolean value indicating whether the option is present or not.
}
\value{
\code{argParse} is used for the side effects.It returns (invisibly) the
number of parameters it parsed.

\code{argGet} and \code{argGetPos} returns a character
string. \code{argPresent} returns a boolean value.

In case of any error (wrong syntax, or not-existing option) the R
session quits while printing the error message.
}
\references{
  The BIOS file \code{arg.h}, \code{arg.c} and the manual \url{http://bioinfo.bas.roche.com:8080/bios/common/libprpi/arg.hdoc}.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}

\examples{
\dontrun{
argParse("verbose threshold,2", "infile outfile", usage="prog [-infile
]infile [-outfile ]outfile [-verbose] [-threshold MIN MAX]")
argPresent("verbose")
argGet("threshold") ## ==argGetPos("threshold", 1)
argGetPos("threshold",2)
argGet("infile")
argGet("outfile")
}
}
