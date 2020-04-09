writeDgeTables <- function(edgeResult, outdir=getwd()) {
  contrasts <- contrastNames(edgeResult)
  outfiles <- file.path(outdir,
                        sprintf("topTable-%s.txt", contrasts))
  tables <- lapply(contrasts, function(x) dgeTable(edgeResult, x))
  write.tableList(tables, outfiles, row.names=TRUE)
}

writeTruncatedDgeTables <- function(edgeResult, outdir=getwd()) {
    contrasts <- contrastNames(edgeResult)
    lapply(contrasts, function(x) {
               tbl <- dgeTable(edgeResult, x)
               degs <- truncateDgeTable(tbl)
               writeMatrix(degs$pos,
                           file.path(outdir,
                                     sprintf("TruncatedDEGtable-positive-%s.txt", 
                                             x)),
                           row.names=FALSE)
               ribiosIO::writeMatrix(degs$neg,
                           file.path(outdir,
                                     sprintf("TruncatedDEGtable-negative-%s.txt", 
                                             x)))
           })
    return(invisible(NULL))
}

#' @include AllClasses.R AllGenerics.R AllMethods.R
NULL

#' Export dgeTest results
#' 
#' @param dgeTest A \code{EdgeResult} object
#' @param outRootDir Character string, output directory
#' @param action Character string, what happens if the output directory exists
#' 
#' @importFrom ribiosUtils overwriteDir createDir
#' @importFrom ribiosIO writeMatrix
#' @export
exportEdgeResult <- function(edgeResult, outRootDir,
                                 action=c("ask", "overwrite",
                                          "append", "no")) {
  ow <- ribiosUtils::overwriteDir(outRootDir, action=action)
  if(isFALSE(ow)) {
    return(invisible(NULL))
  }
  
  ## input data
  inputDir <- file.path(outRootDir, "input-data")
  ribiosUtils::createDir(inputDir)
  countsUnfiltered <- dgeList(edgeResult)$counts.unfiltered
  fDataUnfiltered <- dgeList(edgeResult)$genes.unfiltered
  
  writeGct(countsUnfiltered,
           file.path(inputDir, "counts.gct"))
  ribiosIO::writeMatrix(designMatrix(edgeResult),
              file.path(inputDir, "designMatrix.txt"))
  ribiosIO::writeMatrix(contrastMatrix(edgeResult),
              file.path(inputDir, "contrastMatrix.txt"))
  ribiosIO::writeMatrix(fDataUnfiltered,
              file.path(inputDir, "featureData.txt"))
  ribiosIO::writeMatrix(pData(edgeResult),
              file.path(inputDir, "phenoData.txt"))
  
  ## filtering
  filterDir <- file.path(outRootDir, "filtered-data")
  ribiosUtils::createDir(filterDir)
  writeGct(counts(edgeResult),
           file.path(filterDir, "filteredCounts.gct"))
  ribiosIO::writeMatrix(fData(edgeResult),
              file.path(filterDir, "filteredFeatureData.txt"))
  
  ## dge tables
  dgeDir <- file.path(outRootDir, "dgeTables")
  ribiosUtils::createDir(dgeDir)
  writeDgeTables(edgeResult, outdir=dgeDir)
  
  ## truncated dgeTables
  truncDir <- file.path(outRootDir, "truncated-dgeTables")
  ribiosUtils::createDir(truncDir)
  writeTruncatedDgeTables(edgeResult, outdir=truncDir)
  
  ## RData
  rdataDir <- file.path(outRootDir, "RData")
  ribiosUtils::createDir(rdataDir)
  save(edgeResult, 
       file=file.path(rdataDir, "ngsDge.RData"))
  
  ## dgeCounts
  statdir <- file.path(outRootDir, "statistics")
  ribiosUtils::createDir(statdir)
  ribiosIO::writeMatrix(sigGeneCounts(edgeResult), 
              file=file.path(statdir, "ngs-diffGeneCounts.txt"),
              row.names=TRUE)
  lfc <- logFCmatrix(edgeResult)
  lfcPearson <- cor(lfc, use="complete.obs", method="pearson")
  lfcSpearman <- cor(lfc, use="complete.obs", method="spearman")
  ribiosIO::writeMatrix(lfc, 
              file=file.path(statdir, "logFCmatrix.txt"))
  ribiosIO::writeMatrix(lfcPearson,
              file=file.path(statdir, "logFCmatrix-PearsonCorrelation.txt"))
  ribiosIO::writeMatrix(lfcSpearman,
              file=file.path(statdir, "logFCmatrix-SpearmanCorrelation.txt"))
}
