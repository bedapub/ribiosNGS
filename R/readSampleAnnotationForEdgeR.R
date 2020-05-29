#' Read sample annotation from tab-delimited file for EdgeR analysis
#' @param sampleNames Character string, giving sample names
#' @param file Character string, path to a tab-delimited file, or \code{NULL}. The first column must be 
#'   either row names (namely no colum name), or sample names in the same order
#'   of \code{sampleNames}. 
#' @return A \code{data.frame} containing sample annotation, removing
#'   `lib.size`, and `norm.factors` because they will be added
#'   by the edgeR pipeline
#'   
#' @importFrom ribiosExpression readSampleAnnotationFile
#' @export
#' @examples 
#' phenoDataFile <- system.file("extdata/phenoData/test-phenoData.txt",
#'   package="ribiosNGS")
#' readSampleAnnotationFileForEdgeR(phenoDataFile)
#' readSampleAnnotationFileForEdgeR(file=NULL, sampleNames=as.character(1:4))
readSampleAnnotationForEdgeR <- function(sampleNames, 
                                             file=NULL, ...) {
  stopifnot((!is.null(file) && file.exists(file)) | !is.null(sampleNames))
  sampleNames <- as.character(sampleNames)
  if (!is.null(file) && file.exists(file)) {
    pdAll <- ribiosExpression::readSampleAnnotationFile(file, ...)
    pdDropCols <- c("lib.size", "norm.factors")
    pd <- pdAll[,!colnames(pdAll) %in% pdDropCols, drop=FALSE]
    if(!setequal(sampleNames, pd[,1L])) {
      stop("sampleNames do not match the first column of sample annotation file")
    }
    if(!identical(sampleNames, pd[, 1L])) {
      pd <- matchColumn(sampleNames, pd, 1L)
    }
  } else {
    pd <- data.frame(SampleName=sampleNames,
                     row.names=sampleNames)
  }
  return(pd)
}
