\name{formatGmt-methods}
\docType{methods}
\alias{formatGmt-methods}
\alias{formatGmt,character,character,character-method}
\alias{formatGmt,character,character,list-method}
\alias{formatGmt,character,missing,character-method}
\alias{formatGmt,character,missing,list-method}
\title{ Make GMT format strings}
\description{
  Make GMT format strings with titles, comments and genes
}
\section{Methods}{
\describe{

\item{\code{signature(title = "character", comment = "character", genes = "character")}}{
  For one line of GMT: containing one title, one comment and a vector
  character of genes
}

\item{\code{signature(title = "character", comment = "character", genes = "list")}}{
  For multiple lines of GMT: containing titles, comments and a list of
  genes. Each item of the list is a vector of character. The title and
  the genes list must be of the same length. The length of comment can
  be equal to that of the title, or can be one; in the latter case, the
  comment will be replicated into each line of the GMT strings.
}

\item{\code{signature(title = "character", comment = "missing", genes =
    "character")}}{
    When comment is missing, it is set to empty string

}

\item{\code{signature(title = "character", comment = "missing", genes =
    "list")}}{
      When comment is missing, it is set to empty string
}
}}
\keyword{methods}

