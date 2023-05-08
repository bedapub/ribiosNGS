#' Get GCT filename from a directory
#' @param dir Character string, path to a directory where a GCT file is saved
#' @return Character string, full name of the GCT file
#' If no file is found, the function reports an error. If more than one file is
#' found, a warning message is raised, and only the first file is used.
gctFilename <- function(dir) {
  gctFilePattern <- ".*.gct"
  gctFile <- dir(dir, pattern=gctFilePattern, full.names=TRUE)
  if(length(gctFile)>1) {
    warning(sprintf("More than one GCT file detected (%s)- only the first one (%s) is used":
                      paste(gctFile, collapse=","),
                    gctFile[1]))
    gctFile <- gctFile[1]
  } else if (length(gctFile) == 0 || !file.exists(gctFile)) {
    stop(paste0("GCT file with the pattern'", gctFilePattern, 
                "' does not exist in ",
                dir, "!"))
  }
  return(gctFile)
}

#' Read mpsnake output directory into a DGEList object
#' @param dir Character string, path of mpsnake pipeline directory (or the \code{results} subdirectory).
#' @param minReads Integer, minimalistic read numbers for a sample to be considered
#' @return A \code{DGEList} object containing counts, gene, and sample annotation
#' @examples
#' mpsnakeDir <- system.file("extdata/mpsnake-minimal-outdir", package="ribiosNGS")
#' mpsDgeList <- readMpsnakeAsDGEList(mpsnakeDir)
#' 
#' ## equivalent
#' mpsnakeResDir <- system.file("extdata/mpsnake-minimal-outdir", "results",
#'   package="ribiosNGS")
#' mpsDgeList <- readMpsnakeAsDGEList(mpsnakeResDir)
#' @importFrom ribiosIO readTable
#' @importFrom ribiosUtils assertFile isDir
#' @export
readMpsnakeAsDGEList <- function(dir, minReads=1E6) {
  if(ribiosUtils::isDir(file.path(dir, "results"))) {
    dir <- file.path(dir, "results")
  }
  if(!ribiosUtils::isDir(file.path(dir, "annot")) || !ribiosUtils::isDir(file.path(dir, "gct"))) {
    stop(sprintf("Not found in %s: `annot` and `gct` subdirectories.", dir))
  }
  ribiosUtils::assertFile(featFile <- file.path(dir, "annot/feature.annot"))
  feat <- ribiosIO::readTable(featFile, row.names=FALSE)
  samples <- readBiokitPhenodata(dir)
  gctFile <- gctFilename(file.path(dir, "gct"))
  gct <- read_gct_matrix(gctFile)
  res <- edgeR::DGEList(counts=gct,
                        samples=samples,
                        genes=feat,
                        group=samples$group)
  
  too_few_reads <- res$samples$lib.size < minReads
  if(any(too_few_reads)) {
    warning("Following ",
            sum(too_few_reads),
            " samples removed due to too few reads:",
            paste(sampleNames(res)[too_few_reads], collapse=","))
    res <- res[, !too_few_reads]
  }
  return(res)
}

#' Read Illumina MolPhen sample sheet from XLS files
#' @param file A XLS/XLSX file containing in the first sheet
#'  the sample sheet of a molecular phenotyping experiment
#' @return A \code{data.frame} annotating the samples
#' @export
read_illumina_sampleSheet_xls <- function(file) {
  content <- readxl::read_excel(file, col_names=FALSE, 
                                .name_repair = "minimal")
  tmpf <- tempfile()
  ribiosIO::writeMatrix(content, tmpf, row.names = FALSE)
  res <- ribiosIO::read_illumina_sampleSheet(tmpf,
                                             sep="\t")
  return(res)
}


##----------------------##
## deprecated funcs
##----------------------##

#' Parse feature information from molecular-phenotyping GCT files
#' @param gctMatrix A \code{GctMatrix} capturing the counts of 
#'   a molecular phenotyping experiment
#' @return A data.frame with following columns: \code{GeneID} (as integer), 
#'   \code{GeneSymbol} (as character), and \code{Transcript},
#'    with the original names as row names
#' @seealso \code{\link{readMolPhenCoverageGct}}, which calls this function
#'   internally to parse molecular phenotyping gene features
#' @export
parseMolPhenFeat <- function(gctMatrix) {
  featureNames <- rownames(gctMatrix)
  fsplit <- strsplit(featureNames, ";")
  gs <- sapply(fsplit, "[[", 1L)
  gid <- gsub("EntrezGeneID=", "", sapply(fsplit, "[[", 2L))
  transcript <- ribiosIO::gctDesc(gctMatrix)
  res <- data.frame(GeneID=as.integer(gid),
                    GeneSymbol=I(gs),
                    Transcript=I(transcript),
                    row.names=featureNames)
  return(res)
}

#' Read molecular phenotyping coverage file
#' @param file Character string, a coverage GCT file of a molecular 
#'   phenotyping experiment.
#' @return A list of two elements: \code{coverage}, which represents the
#'    coverage matrix, and \code{genes}, which represents feature annotation.
#' @examples 
#' mpsCov <- readMolPhenCoverageGct(system.file(file.path("extdata",
#'     "AmpliSeq_files",
#'     "MolPhen-coverage-example-20200115.gct"), 
#'   package="ribiosNGS"))
#' @export
readMolPhenCoverageGct <- function(file) {
  mat <- ribiosIO::read_gct_matrix(file)
  fData <- parseMolPhenFeat(mat)
  res <- list(coverage=mat, genes=fData)
  return(res)
}

#' Read molecular phenotyping output folder into a DGEList object
#' @param dir Path of molecular phenotyping output folder, generated by the mpsnake tool.
#' @return A \code{DGEList} object containing counts, gene and sample annotation.
#' @examples 
#' #todo
#' @export
readMolPhenAsDGEList <- function(dir) {
  .Deprecated("readMpsnakeAsDGEList",
              msg=paste("readMolPhenAsDGEList works with older mpsnake versions (<0.7).",
              "For results of newer versions of mpsnake, use radMpsnakeAsDGEList"))
  assertDir(dir)
  gctFile <- gctFilename(file.path(dir, "gct"))
  counts <- readMolPhenCoverageGct(gctFile)
  
  phenoFile <- file.path(dir, "anno", "phenoData.meta")
  if(length(phenoFile)==0 || !file.exists(phenoFile)) {
    stop(sprintf("Sample annotation file '%s' not found ",
                 phenoFile))
  }
  pheno <- ribiosIO::readMatrix(phenoFile, row.names=TRUE, as.matrix=FALSE)
  res <- edgeR::DGEList(counts=counts$coverage,
                        samples=pheno,
                        genes=counts$genes)
  return(res)
} 

