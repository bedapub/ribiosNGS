\name{gtiChiptypes}
\alias{gtiChiptypes}
\alias{gtiChipnames}
\alias{gtiArraytypes}
\alias{raceChiptypes}
\alias{raceChipnames}
\alias{affychipNames}
\title{
  List chip names supported by the GTI system
}
\description{
  The connects to the backend Oracle database, and reads a list of chip
  names that are supported by the GTI system. These names are returned
  as a vector of characters by default, or in a data.frame with
  additional information of description.
}
\usage{
gtiChiptypes(include.desc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{include.desc}{Whether description should be returned as well}
}
\value{
  If \code{include.desc} set to \code{FALSE}, a character vector of chip
  names that are supported by the GTI system.

  Otherwise, a data.frame with chip names and several columns of
  descriptions, including species, technology and descriptions.
}
\note{
  From package version 1.0-15, gtiChiptypes uses genome.CHIP_ARRAY_TYPES@bin table to report
  GTI-supported chip types. Comparing previous versions, the default
  behavior is not changing. By settign 'include.desc=TRUE', more details
  are returned, includeing Technology and Species.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com> with inputs from Laura
  Badi <l.badi@roche.com>
}
\examples{
options(error=utils::recover)

gtiChiptypes()
gtiChiptypes(include.desc=TRUE)

options(error=NULL)
}
