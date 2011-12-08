\name{iofile}
\alias{iofile}
\title{
  Shortcut function to specify a file directory for data import/export
}
\description{
  Quite often we need to import and export data (especially bulky files)
  into a directory other than the local file. This function is a
  shortcut to get full names of import/export files.
}
\usage{
iofile(x)
}
\arguments{
  \item{x}{File or directory name(s) to be read in from the
    \code{DATA_DIR}. If missing, \code{DATA_DIR} is returned.}
}
\details{
   The function first determines whether the global variable
   \code{DATA_DIR} exists. If yes, the value of \code{DATA_DIR} will be
   used as the directory from/to which input/export files should be
   read/written.

   If \code{DATA_DIR} does not exist in \code{.GlobalEnv}, the function
   tries to use a folder named \code{data} in the current working
   directory as \code{DATA_DIR}. If this local folder exists, its name
   will be assigned to the \code{DATA_DIR} \emph{global} variable. If
   the folder does not exist, the function will exit.

   The steps above ganrantee that there is a global variable named
   \code{DATA_DIR}, pointing to a directory where files are read from or
   written to.

   The parameter \code{x} can be file or directory names in the
   \code{DATA_DIR} directory. In this case, \code{iofile(x)} returns
   their full names. When \code{x} is omitted, \code{iofile()} returns
   the value of \code{DATA_DIR}. A common usage for the later case is \code{dir(iofile())}.
}
\value{
  Character or character vectors, of either full names of files in the
  \code{DATA_DIR}, or the name of \code{DATA_DIR} itself.
}
\author{Jitao David Zhang <jitao_david.zhang@roche.com>}
\examples{
DATA_DIR <- system.file("extdata", package="ribiosIO")
dir(iofile())
readLines(iofile("test.gct"), n=2)
}
