\name{buildGSEAcomm}
\alias{buildGSEAcomm}
\title{
  Build bash scripts to run GSEA from command line
}
\description{
  With the help of \code{buildGSEAcomm}, user can setup bash scripts
  required for GSEA analysis from R, which can be then executed from
  command-line or submitted to server cluster. Using the function can
  significantly reduce the time and effort setting up an analysis
  pipeline including GSEA. In addition it circumvents the problem of
  file names containing minus signs, which is problematic for the native
  GSEA program.
}
\usage{
buildGSEAcomm(rnkFiles, gmtFile = DEFAULT_GMT, chipFile, nperm = 1000L, collapse = FALSE, plotTopX = 25, outdir = "./", addShebang = TRUE)
}
\arguments{
  \item{rnkFiles}{Two-column rank files as GSEA input: genes and
    statistics, without quoting, tab-delimited.}
  \item{gmtFile}{Gmt file of gene signatures, by default we use the
    RoNet GMT file.}
  \item{chipFile}{Chip type used to translate identifiers. If missing,
    the input rnk file must have Gene Symbols as identifiers.}
  \item{nperm}{Number of permutations: currently only feature-permutatin
    is supported.}
  \item{collapse}{Logical, whether features should be collapsed or
    not. For unique features please set it to \code{FALSE}.}
  \item{plotTopX}{Integer, deciding how many top-ranked pathways should
    be plotted with details}
  \item{outdir}{Output directory of GSEA analysis results: will be
    created if not existing}
  \item{addShebang}{Logical, whether the shebang (hashbang) line should
    be added to the output text so that it can be directly written into
    file and executed from command-line.}
}
\details{
  The current native JAVA implementation of the GSEA program do not
  support files with minus signs (\sQuote{-}) in their names. Using such
  files will lead to strange behaviour or failing of the programm. The
  \code{buildGSEAcomm} command addresses this issue by generating soft
  links to these files. The temporary link files will be removed after
  the GSEA procedure and users do not have to care for them.
}
\value{
  Character string vectors, of the same length as the input rnk
  files. 
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>
}
\examples{
rnkFiles <- system.file("extdata", c("test-1.rnk", "test-2.rnk"),
package="ribiosGSEA")
gmt <- system.file("extdata", "20120716-path.ronet.roche.symbols.gmt",
package="ribiosGSEA")
chip <- system.file("extdata", "20120716-GENE_SYMBOL.chip", package="ribiosGSEA")
(comms <- buildGSEAcomm(rnkFiles, gmtFile = gmt, chip = chip, nperm = 1000L, collapse = FALSE, plotTopX = 25, outdir = "./", addShebang = TRUE))
cat(comms[1])
}
