#' Write an DGEList object as plain files for downstream analysis
#' @param dgeList An DGEList object
#' @param exprs.file File name where counts are saved
#' @param fData.file File name where feature annotations are saved
#' @param pData.file File name where sample annotations are saved
#' @param group.file File name where the sample group information is saved
#' @param groupLevels.file File where the sample group levels are saved
#' @param feat.name Feature names. Can be a column name in \code{genes} of the DGEList object, or a vector of the same length as the fetaures. If \code{NULL}, row names of the count matrix are used.
#' @param feat.desc Feature descriptions, used in GCT files. If \code{NULL}, 'GeneSymbol' will be used if the column is present, otherwise no description will be used
#' 
#' Expression values are saved by default in the gct format, unless the file name ends with tsv in which case a tab-separated value (TSV) file will be saved.
#' 
#' Sample group and group level information are derived from the \code{group} column of the sample annotation.
#' 
#' @note 
#' In case the input matrix has no feature name, the feature names are set to be the integer array starting from 1.
#' 
#' In case no \code{genes} item is available in the DGEList, a minimal data.frame containing one column, \code{Feature}, is exported with row names of the count matrix used as both row names as well as the content of the \code{Feature} column.
#' 
#' @examples 
#' 
#' y <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' d <- DGEList(counts=y, group=rep(1:2,each=2))
#' 
#' exprsFile <- tempfile()
#' fDataFile <- tempfile()
#' pDataFile <- tempfile()
#' groupFile <- tempfile()
#' groupLevelsFile <- tempfile()
#' writeDGEList(d, exprs.file=exprsFile, fData.file=fDataFile, pData.file=pDataFile, 
#'   group.file=groupFile, groupLevels.file=groupLevelsFile)
#' 
#' head(read_gct_matrix(exprsFile))
#' head(ribiosIO::readMatrix(fDataFile))
#' head(ribiosIO::readMatrix(pDataFile))
#' head(readLines(groupFile))
#' head(readLines(groupLevelFile))
writeDGEList <- function(dgeList, exprs.file, fData.file, pData.file, 
                         group.file,
                         groupLevels.file,
                         feat.name = NULL, feat.desc = NULL) {
  countMat <- dgeList$counts
  fData <- dgeList$genes
  if(is.null(fData)) {
    fData <- data.frame(Feature=rownames(countMat), row.names=rownames(countMat))
  }
  sampleAnno <- dgeList$samples

  exprs.file.format <- ifelse(grepl("tsv$", exprs.file),
                              "tsv", "gct")
  if (is.null(feat.name)) {
    feat.name <- rownames(countMat)
    if(is.null(feat.name))
      feat.name <- 1:nrow(countMat)
  } else {
    if (is.character(feat.name) && length(feat.name) == 1) {
      stopifnot(feat.name %in% colnames(fData))
      feat.name <- fData[, feat.name]
    }
  }
  rownames(countMat) <- feat.name
  rownames(fData) <- feat.name
  if (exprs.file.format == "tsv") {
    ribiosIO::writeMatrix(countMat, exprs.file, row.names = TRUE)
  }
  else if (exprs.file.format == "gct") {
    if(is.null(feat.desc) && "GeneSymbol" %in% colnames(fData))
      feat.desc <- "GeneSymbol"
    if (!is.null(feat.desc)) {
      if (is.character(feat.desc) && length(feat.desc) == 
          1) {
        stopifnot(feat.desc %in% colnames(fData))
        feat.desc <- fData[, feat.desc]
      }
    }
    ribiosIO::write_gct(countMat, exprs.file, feat.name = feat.name, 
                        feat.desc = feat.desc)
  }
  else {
    stop("Should not be here")
  }
  if (!missing(fData.file) && !is.null(fData.file)) {
    writeMatrix(fData, fData.file)
  }
  if (!missing(pData.file) && !is.null(pData.file)) {
    writeMatrix(sampleAnno, pData.file)
  }
  if (!missing(group.file) && !is.null(group.file)) {
    writeLines(as.character(sampleAnno$group), group.file)
  }
  if (!missing(groupLevels.file) && !is.null(groupLevels.file)) {
    writeLines(levels(sampleAnno$group), groupLevels.file)
  }
}
