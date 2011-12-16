\name{raceChiptypes}
\alias{raceChiptypes}
\alias{raceChipnames}
\alias{affychipNames}
\title{
  List chip names supported by the RACE system
}
\description{
  The connects to the backend Oracle database, and reads a list of chip
  names that are supported by the RACE system. These names are returned
  as a vector of characters by default, or in a data.frame with
  descriptional information.
}
\usage{
raceChiptypes(include.desc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{include.desc}{Whether description should be returned as well}
}
\value{
  If \code{include.desc} set to \code{FALSE}, a character vector of chip
  names that are supported by the RACE system.

  Otherwise, a data.frame with chip names and descriptions.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com> with inputs from Laura
  Badi <l.badi@roche.com>
}
\examples{
\dontrun{
raceChiptypes()
raceChiptypes(include.desc=TRUE)
}
}
