\name{splitISAModules}
\alias{splitISAModules}
\title{
  Split ISA modules by correlations of features, conditions or both
}
\description{
  An \code{ISAModules} object contains reversely correlated features and
  samples. This function splits features, samples or both simultaneously
  to make sub-modules that are expressed in positive correlation.
}
\usage{
splitISAModules(modules, module,splitFeatures=TRUE, splitConditions=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modules}{An \code{ISAModules} object, mandatory}
  \item{module}{Module index, optional. In case omitted, all modules
    will be split.}
  \item{splitFeatures}{Logical. If set to \code{TRUE}, anti-correlated
    features will be split into two modules
  }
  \item{splitConditions}{Logical. If set to \code{TRUE}, anti-correlated
    conditions will be split into two modules
  }
}
\details{
  The function is usually applied to \code{ISAModules} in
  prognosis/signature settings.
}
\value{
  An edited \code{ISAModules} object (of the same class as the input
  \code{modules} object), with \code{genes}, \code{conditions} and
  \code{seeddata} slots splitted. The \code{rundata} slot is unaffected
  and preserved.
}
\examples{
library(ALL)
data(ALL)
set.seed(1887)
modules <- ISA(ALL, thr.gene=2.7, thr.cond=1.4)

## split anti-correlated features and conditions
modules.splitBoth <- splitISAModules(modules, splitFeatures=TRUE,
splitConditions=TRUE)

## split anti-correlated features only, so that all features in one module are positively correlated
modules.splitFeature <- splitISAModules(modules, splitFeatures=TRUE,
splitConditions=FALSE)

## split anti-correlated samples only
modules.splitCondition <- splitISAModules(modules, splitFeatures=FALSE,
splitConditions=TRUE)

## only split a few modules
submodules.splitFeature <- splitISAModules(modules, module=1,
splitFeatures=TRUE, splitConditions=FALSE)
}
