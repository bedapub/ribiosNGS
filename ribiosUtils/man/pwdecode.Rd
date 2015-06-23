\name{pwdecode}
\alias{pwdecode}
\title{
  Decode password with function implemented with pwencode
}
\description{
  Decode password encypted with pwencode.
}
\usage{
pwdecode(password)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{password}{Character string to be decoded. If starting with a
    empty character, the string is sent for decoding; otherwise, it is
    deemed as clear text password and returned.}
}
\details{
  See pwdecode function documentation in BIOS for implemetnation
  details.

  Note that since R does not support strings embedding null values
  (\code{\\000}), the password to be decoded has to be given with two
slashes, e.g. \sQuote{ \\\\001\\\\000\\\\129\\\\235}.
}
\value{
  Decoded character string, or empty string if decoding fails
}
\references{
  BIOS C module list, \url{http://bioinfo.bas.roche.com:8080/bios/common/modulelist.html}
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>. The C library code
  was written by Detlef Wolf.
}
\examples{
mycode <- " \\\\001\\\\000\\\\141\\\\314\\\\033\\\\033\\\\033\\\\033\\\\033\\\\142\\\\303\\\\056\\\\166\\\\311\\\\037\\\\042"
pwdecode(mycode)
}
