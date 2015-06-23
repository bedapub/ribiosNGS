\name{verbose}
\alias{verbose}
\title{
  Print messages conditional on the verbose level
}
\description{
  The verbose level can be represented by non-negative integers. The
  larger the number is, the more verbose is the program: it prints then
  more messages for users' information.
  
  This function decides whether or not to print a message, dependent on the global
  verbose level and the specific level of the message. If the specific
  level is larger than the global level, the message is suppresed;
  otherwise it is printed. see the details section for an example.
}
\usage{
verbose(..., global = 1L, this = 1L)
}
\arguments{
  \item{\dots}{Messages to be printed, will be passed to the
    \code{message} function}
  \item{global}{Integer, the global verbose level}
  \item{this}{Integer, the verbose level of this message}
}
\details{
  Suppose the global verbose level is set to \code{5}, and two messages
  have levels of \code{1} and \code{7} repsectively. Since \code{1}
  suggests a low-threshold of being verbose, the first message is
  printed; whereas the message of level \code{7} is only printed when
  the program should run in a more verbose way (\code{7,8,9,\dots}), it
  is suppressed in the current global verbose level.
}
\value{
  The function is used for its side effect by printing messages.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
Gv <- 5L
verbose("Slightly verbosing", global=Gv, this=1L)
verbose("Moderately verbosing", global=Gv, this=5L)
verbose("Heavily verbosing", global=Gv, this=9L)
}

