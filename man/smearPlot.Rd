% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/plotMethods.R
\name{smearPlot}
\alias{smearPlot}
\alias{smearPlot,EdgeResult-method}
\title{Smear plot}
\usage{
smearPlot(object, ...)

\S4method{smearPlot}{EdgeResult}(
  object,
  contrast = NULL,
  freeRelation = FALSE,
  xlab = "Average logCPM",
  ylab = "logFC",
  pch = 19,
  cex = 0.2,
  smearWidth = 0.5,
  panel.first = grid(),
  smooth.scatter = FALSE,
  lowess = FALSE,
  multipage = FALSE,
  ...
)
}
\arguments{
\item{object}{An object}

\item{...}{Other parameters}

\item{contrast}{Character, contrast of interest}

\item{freeRelation}{Logical}

\item{xlab}{Character}

\item{ylab}{Character}

\item{pch}{Character or integer}

\item{cex}{Numeric}

\item{smearWidth}{Numeric}

\item{panel.first}{Grid}

\item{smooth.scatter}{Logical}

\item{lowess}{Logical}

\item{multipage}{Logical}
}
\description{
Smear plot
}
\section{Methods (by class)}{
\itemize{
\item \code{smearPlot(EdgeResult)}: Method for EdgeResult

}}
