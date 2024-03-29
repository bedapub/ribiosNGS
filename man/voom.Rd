% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/AllMethods.R
\name{voom}
\alias{voom}
\alias{voom,DGEList-method}
\alias{voom,matrix-method}
\alias{voom,ExpressionSet-method}
\alias{voom,EdgeObject-method}
\title{Perform VOOM analysis}
\usage{
voom(object, ...)

\S4method{voom}{DGEList}(object, ...)

\S4method{voom}{matrix}(object, ...)

\S4method{voom}{ExpressionSet}(object, ...)

\S4method{voom}{EdgeObject}(object, ...)
}
\arguments{
\item{object}{An object}

\item{...}{Other parameters}
}
\description{
Perform VOOM analysis
}
\section{Methods (by class)}{
\itemize{
\item \code{voom(DGEList)}: Method for DGEList

\item \code{voom(matrix)}: Method for matrix

\item \code{voom(ExpressionSet)}: Method for matrix

\item \code{voom(EdgeObject)}: Method for EdgeObject, norm.factors are calculated first if not done yet

}}
