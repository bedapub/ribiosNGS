#' Read sample annotation from tab-delimited file for EdgeR analysis
#' @param sampleNames Character string, giving sample names
#' @param file Character string, path to a tab-delimited file, or \code{NULL}. The first column must be 
#'   either row names (namely no colum name), or sample names in the same order
#'   of \code{sampleNames}. 
#' @return A \code{data.frame} containing sample annotation, removing
#'   `lib.size`, and `norm.factors` because they will be added
#'   by the edgeR pipeline
#'   
#' Either \code{file} or \code{sampleNames} must be provided. If both provided,
#' information in \code{file} has priority.
#' 
#' @importFrom Biobase read.AnnotatedDataFrame
#' @importFrom methods as
#' @importFrom ribiosUtils putColsFirst
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
  if (file.exists(file)) {
    pdAnnoDf <- Biobase::read.AnnotatedDataFrame(file, ...)
    pdAll <- methods::as(pdAnnoDf, "data.frame")
    pdDropCols <- c("lib.size", "norm.factors")
    pd <- pdAll[,!colnames(pdAll) %in% pdDropCols, drop=FALSE]
    haltifnot(identical(sampleNames, pd[,1L]),
              msg="sampleNames do not match the first column")
    if("SampleName" %in% colnames(pd)[-1]) {
      warning("The original SampleName column will be named as 'SampleName.1`")
      colnames(pd)[colnames(pd)=="SampleName"] <- "SampleName.1"
    }
    pd$SampleName <- as.character(pd[,1])
    pd <- ribiosUtils::putColsFirst(pd, "SampleName")
  } else {
    pd <- data.frame(SampleName=sampleNames,
                     row.names=sampleNames)
  }
  return(pd)
}
