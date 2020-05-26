#' Read sample annotation from tab-delimited file for EdgeR analysis
#' @param file Character string, a tab-delimited file. Row names must be present.
#'   If \code{NULL}, \code{sampleNames} must be provided.
#' @param sampleNames Character string, giving sample names. If \code{NULL}, 
#'   \code{file} must be provided.
#' @return A \code{data.frame} containing sample annotation, removing
#'   `group`, `lib.size`, and `norm.factors` because they will be added
#'   by the edgeR pipeline
#'   
#' Either \code{file} or \code{sampleNames} must be provided. If both provided,
#' information in \code{file} has priority.
#' 
#' @importFrom Biobase read.AnnotatedDataFrame
#' @importFrom methods as
#' @export
#' @examples 
#' phenoDataFile <- system.file("extdata/phenoData/test-phenoData.txt",
#'   package="ribiosNGS")
#' readSampleAnnotationFileForEdgeR(phenoDataFile)
#' readSampleAnnotationFileForEdgeR(file=NULL, sampleNames=as.character(1:4))
readSampleAnnotationFileForEdgeR <- function(file=NULL,
                                             sampleNames = NULL) {
  stopifnot((!is.null(file) && file.exists(file)) | !is.null(sampleNames))
  if (file.exists(file)) {
    pdAnnoDf <- Biobase::read.AnnotatedDataFrame(file)
    pdAll <- methods::as(pdAnnoDf, "data.frame")
    pdDropCols <- c("group", "lib.size", "norm.factors")
    pd <- pdAll[,!colnames(pdAll) %in% pdDropCols]
  } else {
    pd <- data.frame(Sample=sampleNames)
  }
  return(pd)
}
