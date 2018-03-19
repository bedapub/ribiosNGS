#' Export an ExpressionSet object as tab-delimited (or gct) files
#'
#' @param eset The \code{eSet} object to be exported
#' @param exprs.file Character, file name where \code{exprs} data is written to
#' @param fData.file Character, optional, file name where \code{fData} data is written to
#' @param pData.file Character, optional, file name where \code{pData} data is written to
#' @param exprs.file.format Character, write \code{exprs} data in either \code{gct} or \code{tsv} format 
#' @param feat.name Character, feature names or a column in \code{fData} to get feature names. If \code{NULL}, feature names of the \code{eSet} object will be used. Note that if not \code{NULL}, row names of both \code{exprs} and \code{fData} will be overwritten by the provided \code{feat.name}.
#' @param feat.desc Character, feature descriptions or a column in \code{fData} to get feature descriptions. If \code{NULL}, the column in the \code{gct} file will be empty. Only used if \code{exprs.file.format} is \code{gct}.
#' 
#' \code{readEset} and \code{writeEset} provide a lightweighted mechanism to 
#' import/export essential information from/to plain text files. 
#' They can use up to three tab-delimited files to store information of
#' an \code{eSet} (oftenly used is its subclass, \code{ExpressionSet}) object: 
#' a file holding the expression matrix as returned by the \code{\link{exprs}} 
#' function (\code{exprs.file}), a file containing feature annotations as 
#' returned by the \code{\link{fData}} function (\code{fData.file}), 
#' and finally a file containing sample annotations, 
#' as returned by \code{pData} (\code{pData.file}).
#'  
#' @note 
#' One limitation of \code{readEset} and \code{writeEset} functions is that 
#' they only support the export/import of exactly \strong{one} expression 
#' matrix from one \code{ExpressionSet} object. Although an 
#' \code{ExpressionSet} can hold more than one matrices other than the
#' one known as \code{exprs}, they are currently not handled by \code{writeEset} 
#' or \code{readEset}. If such an \code{ExprssionSet} object is first
#' written in plain files, and then read back as an \code{ExpressionSet}, 
#' matrices other than the one accessible by \code{exprs} will be discarded.
#'  
#' Similarly, other pieces of information saved in an \code{ExpressionSet}, 
#' e.g. experimental data, are lost as well after a cycle of exporting 
#' and subsequent importing. If keeping these information is important for you, 
#' other functions should be considered instead of \code{readEset} and 
#' \code{writeEset}, for instance to save an image in a binary file with 
#' the \code{\link{save}} function.
#' 
#' Yet another limitation is that factor information is lost. This hits 
#' especially the phenoData where factor information, such as sample groupping
#' and orders of levels, may be important.
#' 
#' @return NULL, only side effect is used
#' 
#' @seealso \code{\link{readEset}}
#' @examples 
#' data(sample.ExpressionSet)
#' exprs.file <- tempfile()
#' fData.file <- tempfile()
#' pData.file <- tempfile()
#' writeEset(sample.ExpressionSet, exprs.file, fData.file, pData.file,
#' exprs.file.format="gct")
#' writeEset(sample.ExpressionSet, exprs.file, fData.file, pData.file,
#' exprs.file.format="tsv")
writeEset <- function(eset,exprs.file,fData.file,pData.file,
                      exprs.file.format=c("gct", "tsv"),
                      feat.name=NULL, feat.desc=NULL) {
  exprs.file.format <- match.arg(exprs.file.format)
  if(is.null(feat.name)) {
    feat.name <- featureNames(eset)
  } else {
    if(is.character(feat.name) && length(feat.name)==1) {
      stopifnot(feat.name %in% colnames(fData(eset)))
      feat.name <- fData(eset)[, feat.name]
    }
  }
  exprsMat <- exprs(eset)
  rownames(exprsMat) <- feat.name
  fDataDf <- fData(eset)
  rownames(fDataDf) <- feat.name
  if(exprs.file.format=="tsv") {
    ribiosIO::writeMatrix(exprsMat, exprs.file, row.names = TRUE)
  } else if (exprs.file.format=="gct") {
    if(!is.null(feat.desc)) {
      if(is.character(feat.desc) && length(feat.desc)==1) {
        stopifnot(feat.desc %in% colnames(fData(eset)))
        feat.desc <- fData(eset)[, feat.desc]
      }
    }
    ribiosIO::write_gct(exprsMat, exprs.file, feat.name=feat.name, feat.desc=feat.desc)
  } else {
    stop("Should not be here")
  }
  if(!missing(fData.file) && !is.null(fData.file)) {
    writeMatrix(fDataDf, fData.file)
  }
  if(!missing(pData.file) && !is.null(pData.file))
    writeMatrix(pData(eset), pData.file)
}

#' Write sample groups and group levels into plain text files
#' 
#' @param sampleGroups Factor, encoding sample groups.
#' @param sampleGroups.file Character, file name where the information of sample groups is written to.
#' @param sampleGroupLevels.file Character, file name where the information of sample group levels is written to.
#' 
#' The function is used to export sample group and group level information for differential gene expression analysis.
#' 
#' @examples 
#' writeSampleGroups(gl(3,4), stdout(), stdout())
writeSampleGroups <- function(sampleGroups, sampleGroups.file, sampleGroupLevels.file) {
  stopifnot(is.factor(sampleGroups))
  writeLines(as.character(sampleGroups), sampleGroups.file)
  writeLines(levels(sampleGroups), sampleGroupLevels.file)
}

#' Read eSet object from plain files
#' 
#' @param exprs.file Character, file name where \code{exprs} data is written to
#' @param fData.file Character, optional, file name where \code{fData} data is written to
#' @param pData.file Character, optional, file name where \code{pData} data is written to
#' @param exprs.file.format Character, write \code{exprs} data in either \code{gct} or \code{tsv} format 
#' 
#' The function can read in eSet object saved by \code{\link{writeEset}} by parsing
#' three plain text files: \code{exprs.file}, \code{fData.file}, and \code{pData.file}.
#' 
#' Currently both \code{tsv} and \code{gct} formats are supported for expression
#' file.
#' 
#' See \code{writeEset} for limitations of these functions.
#' 
#' @seealso \code{\link{writeEset}}
#' 
#' @examples 
#' data(sample.ExpressionSet)
#' fData(sample.ExpressionSet) <- data.frame(ProbeID=featureNames(sample.ExpressionSet),
#'  row.names=featureNames(sample.ExpressionSet))
#' exprs.file <- tempfile()
#' fData.file <- tempfile()
#' pData.file <- tempfile()
#' writeEset(sample.ExpressionSet, exprs.file, fData.file, pData.file,
#' exprs.file.format="gct")
#' testRead1 <- readEset(exprs.file, fData.file, pData.file, exprs.file.format="gct")
#' writeEset(sample.ExpressionSet, exprs.file, fData.file, pData.file,
#' exprs.file.format="tsv")
#' testRead2 <- readEset(exprs.file, fData.file, pData.file, exprs.file.format="tsv")

readEset <- function(exprs.file,fData.file, pData.file,
                     exprs.file.format=c("gct", "tsv"),
                     sep="\t", header=TRUE, ...) {
  exprs.file.format <- match.arg(exprs.file.format)
  if(exprs.file.format=="gct") {
    ef <- ribiosIO::read_gct_matrix(exprs.file)
  } else if (exprs.file.format=="tsv") {
    ef <- ribiosIO::readMatrix(exprs.file, as.matrix=TRUE)
  }
  if(!missing(fData.file) && !is.null(fData.file)) {
    ff <- readFKtable(fData.file, rownames(ef),
                      sep=sep, header=header,
                      ...)
    fd <- new("AnnotatedDataFrame", ff)
  } else {
    fd <- new("AnnotatedDataFrame",
              data.frame(row.names=rownames(ef))
              )
  }
  if(!missing(pData.file) && !is.null(pData.file)) {
    pf <- readFKtable(pData.file,
                      sep=sep, header=header,
                      colnames(ef), ...)
    pd <- new("AnnotatedDataFrame", pf)
  } else {
    pd <- new("AnnotatedDataFrame",
              data.frame(row.names=colnames(ef))
              )
  }
  eset <- new("ExpressionSet",
              exprs=ef,
              featureData=fd,
              phenoData=pd)
  return(eset)
}
