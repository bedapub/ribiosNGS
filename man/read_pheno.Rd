\name{read_pheno}
\alias{read_pheno}
\title{Read pheno data from CLS or tab-delimited file}
\description{
  Read pheno (sample annotation) data from CLS file or tab-delimited
  file (sample information file)
}
\usage{
read_pheno(file)
}
\arguments{
  \item{file}{A CLS file or tab-delimited file}
}
\value{
  A \code{data.frame} containing sample annotations. In case of
  \code{CLS} input file, the \code{data.frame} contains two columns:
  \code{Array} (indices of arrays) and \code{Class} (classes indexed in
  the GCT file). In case of tab-delimited file, the file will be parsed
  into the \code{data.frame}, assuming the file having column names but
  no row names.
}
\references{
  For tab-delimited file (sample information file), see
  \url{http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/sample-information-file}.

  For CLS file, see
  \url{http://www.broadinstitute.org/cancer/software/genepattern/gp_guides/file-formats/sections/cls}.
  
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\seealso{
  \code{\link{read_cls}} and \code{\link{read.csv}}.
}
\examples{
testClsFile <- system.file("extdata/test.cls", package="ribiosIO")
testPhenoFile <- system.file("extdata/testSampleInfo.txt",
package="ribiosIO")

(clsPheno <- read_pheno(testClsFile))
(txtPheno <- read_pheno(testPhenoFile))

stopifnot(identical(clsPheno, txtPheno))
}
