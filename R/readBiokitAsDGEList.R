check_pattern_matching_file <- function(file, pattern) {
  if(!file.exists(file)) {
    stop(paste0("File with the pattern '", pattern, "' does not exist!"))
  } else if (length(file)>1) {
    stop(paste0("More than one file with the pattern '", pattern, "' were found:",
                paste(file, sep=",")))
  } else {
    return(file)
  }
  stop("Should not be here - please inform the developer")
  return(FALSE)
}

#' Read GCT files from Biokit output directory
#' 
#' @param dir Biokit output directory
#' @param anno Annotation type, either \code{refseq}, \code{ensembl}, 
#' and \code{gencode} is supported
#' @param type GCT file type, \code{count}, \code{tpm}, \code{count_collapsed}, 
#'   \code{tpm_collapsed}, and \code{log2tpm} are supported.
#' @param verbose verbose, if TRUE, verbose mode is turned on.
#' @return A numeric matrix with the attribute \code{desc} encoding the values
#'   in the description column of the GCT format.
#'
#' The function depends on \code{gct} (in case \code{anno="refseq"}) or
#'  \code{gct-ens} (in case \code{anno="ensembl"}) sub-directory in the biokit
#'  output directory.
#'  
#' @examples
#' 
#' ##... (TODO: add a mock output directory in testdata)
#' 
#' @importFrom ribiosIO read_gct_matrix
#' @export readBiokitGctFile
readBiokitGctFile <- function(dir, 
                              anno=c("refseq", "ensembl", "gencode"),
                              type=c("count", "tpm", "count_collapsed", 
                                     "tpm_collapsed", "log2tpm"),
                              verbose=FALSE) {
  anno <- match.arg(anno)  
  type <- match.arg(type)
  
  annoPath <- "gct"
  
  gctDir <- file.path(dir, annoPath) 
  assertDir(gctDir)
  
  fileEndPattern <- switch(type,
                        "count"=".*_count.gct",
                        "tpm"=".*_tpm.gct",
                        "count_collapsed"=".*_count_collapsed.gct",
                        "tpm_collapsed"=".*_tpm_collapsed.gct")
  
  filePattern <- paste0("^", anno, ".*", fileEndPattern, "$")
  
  gctFile <- dir(gctDir, pattern=filePattern, full.names=TRUE)
  check_pattern_matching_file(gctFile, filePattern)
  if(verbose)
    message("Reading GCT file: ", gctFile)
  mat <- ribiosIO::read_gct_matrix(gctFile)
  return(mat)
}

#' Read Biokit phenodata
#' 
#' @param dir Character string, Biokit output directory
#' @param verbose Logical
#' @return A \code{data.frame} with sample annotation in columns, and sample 
#'   names (identical as the names in gct files, character strings) are row 
#'   names. Nmes of the first three columns are fixed:
#'   \enumerate{
#'     \item \code{SampleName}, \code{SampleID} and \code{group} concatenated by 
#'        underscore
#'     \item \code{SampleID}
#'     \item \code{group}
#'   }
#'   
#' The function depends on the \code{annot/phenoData.meta} file in the biokit
#'  output directory.
#'  
#' @export
readBiokitPhenodata <- function(dir, verbose=FALSE) {
  ## read sample annotation from annot/phenoData.meta
  phenoDataFile <- file.path(dir, "annot", "phenoData.meta")
  ribiosUtils::assertFile(phenoDataFile)
  
  ## to have consistent formats of sample annotation, we rename the frist three columns of annot
  if(verbose)
    message("Reading phenodata file: ", phenoDataFile)
  
  annot <- ribiosIO::readTable(phenoDataFile, row.names=FALSE)
  colnames(annot)[1:3] <- c("SampleName", "SampleID", "group")
  annotSampleName <- as.character(annot[, 1L])
  annotSampleId <- annot[, 2L]
  annotSampleGroup <- annot[, 3L]
  rownames(annot) <- annotSampleName
  
  return(annot)
}

utils::globalVariables(c("GeneID", "EnsemblID"))

#' Read feature annotation from Biokit directory
#' 
#' @param dir Character string, a Biokit output directory.
#' @param anno Character, indicating the annotation type.
#' @param verbose Logical
#' @return A \code{data.frame} containing feature annotation, with feature IDs 
#'   as characters in rownames. The data frame contains following columns 
#'   depending on the \code{anno} parameter: 
#'   \enumerate{
#'     \item FeatureName, the primary key of feature name as characters
#'     \item GeneID (refseq only) or EnsemblID (ensembl only)
#'     \item GeneSymbol
#'     \item mean: mean length
#'     \item median: median length
#'     \item longest_isoform: longest isoform
#'     \item merged: total length of merged exons
#'   }
#'   
#' The function depends on the \code{refseq.annot.gz} (\code{ensembl.annot.gz}) 
#' and \code{refseq.geneLength.gz} (\code{ensembl.geneLength.gz}) files in the 
#' biokit directory.
#' 
#' If \code{.annot.gz} file is not found (which can be the case, for instance,
#'   when older biokit output directories are used), feature annotation is
#'   read from the count GCT file. The resulting \code{data.frame} will only 
#'   contain two columns: \code{FeatureName} and \code{Description}.
#'   
#' If \code{.geneLength.gz} file is not found, no gene length information is
#'  appended.
#' 
#' @export
#' @importFrom dplyr mutate select everything
#' @importFrom ribiosIO gctDesc
#' @examples
#' ## TODO add small example files
readBiokitFeatureAnnotation <-
  function(dir, anno = c("refseq", "ensembl", "gencode"), verbose=FALSE) {
    anno <- match.arg(anno)
    annoDir <- file.path(dir, "annot")
   
    annotFilePattern <- paste0("^", anno, ".*\\.annot\\.gz$")
    geneLengthFilePattern <- paste0("^", anno, ".*\\.geneLength\\.gz$")
    
    annotFile <- dir(annoDir, pattern=annotFilePattern, full.names=TRUE)
    lenFile <- dir(annoDir, pattern=geneLengthFilePattern, full.names=TRUE)
    
    check_pattern_matching_file(annotFile, annotFilePattern)
    check_pattern_matching_file(lenFile, geneLengthFilePattern)
  
    if(verbose) {
      message("Reading feature annotation file: ", annotFile)
      message("Reading gene length annotation file: ", lenFile)
    }
    
    if(file.exists(annotFile)) {
      if (anno == "refseq") {
        ## in the current file, some gene names are not present,
        ## they will cause parsing failures
        suppressWarnings(annotTbl <- readr::read_tsv(
          annotFile,
          col_names = c("GeneID", "GeneSymbol", "GeneName"),
          col_types = "icc"
        ))
        annotTbl <- dplyr::mutate(annotTbl, FeatureName=GeneID)
      } else if (anno %in% c("ensembl", "gencode")) {
        suppressWarnings(annotTbl <- readr::read_tsv(
          annotFile,
          col_names = c("EnsemblID", "GeneSymbol", "GeneName"),
          col_types = "cc"
        ))
        annotTbl <- dplyr::mutate(annotTbl, FeatureName=EnsemblID)
      }
      annotTbl <-  annotTbl %>%
        dplyr::select("FeatureName", dplyr::everything())
    } else {
      gctMat <- readBiokitGctFile(dir, anno=anno)
      annotTbl <- data.frame(FeatureName=rownames(gctMat),
                             Description=gctDesc(gctMat),
                             GeneID=rownames(gctMat),
                             GeneSymbol=gctDesc(gctMat))
      if(anno %in% c("ensembl", "gencode")) {
        colnames(annotTbl)[3L] <- "EnsemblID"
      }
    }
    
    if(file.exists(lenFile)) {
      lenTbl <- readr::read_tsv(lenFile,
                                col_names = TRUE,
                                col_types = "cnnnn")
      res <- merge(annotTbl, lenTbl,
                   by.x = "FeatureName", by.y = colnames(lenTbl)[1], all.x=TRUE)
    } else {
      res <- annotTbl
    }
    rownames(res) <- as.character(res$FeatureName)
    return(res)
  }

#' Read a Biokit output directory into a DGEList object for downstream analysis
#' 
#' 
#' @param dir Biokit output directory
#' @param anno Annotation type, either \code{refseq} or \code{ensembl} is
#'   supported
#' @param useCollapsedData Logical, \code{FALSE} as default. If set to 
#'   \code{TRUE}, counts are collapsed by gene symbols. This is not recommended
#'   because gene symbols are not stable identifiers.
#' @param verbose Logical
#' The function depends on \code{gct} (\code{gct-ens}) and \code{annot} 
#' directories of biokit output directory.
#' 
#' @examples
#' 
#' ##... (TODO: add a mock output directory in testdata)
#' 
#' @export readBiokitAsDGEList
readBiokitAsDGEList <- function(dir, 
                                anno=c("refseq", "ensembl", "gencode"),
                                useCollapsedData=FALSE, verbose=FALSE) {
  ## read gct file
  anno <- match.arg(anno)
  if(useCollapsedData) {
    countType <- "count_collapsed"
    tpmType <- "tpm_collapsed"
  } else {
    countType <- "count"
    tpmType <- "tpm"
  }
  countMat <- readBiokitGctFile(dir, anno=anno, type=countType, verbose=verbose)
  tpmMat <- readBiokitGctFile(dir, anno=anno, type=tpmType, verbose=verbose)
  
  stopifnot(identical(colnames(countMat), colnames(tpmMat)))
  if(!identical(rownames(countMat), rownames(tpmMat))) {
    if(setequal(rownames(countMat), rownames(tpmMat))) {
      tpmMat <- tpmMat[as.character(rownames(countMat)),,drop=FALSE]
    } else {
      warning(paste("Count matrix and TPM matrix have different row names!",
        "The TPM matrix is newly organized"))
      commonFeat <- as.character(intersect(rownames(countMat), 
                                           rownames(tpmMat)))
      tpmUniqFeat <- as.character(setdiff(rownames(countMat), 
                                          rownames(tpmMat)))
      tpmNewMat <- countMat
      tpmNewMat[commonFeat,] <- tpmMat[commonFeat,]
      tpmNewMat[tpmUniqFeat,] <- NA
      tpmMat <- tpmNewMat
    }
  }
  stopifnot(identical(rownames(countMat), rownames(tpmMat)))
  
  ## sample annotation
  annot <- readBiokitPhenodata(dir, verbose=verbose)
  annotSampleName <- rownames(annot)
  if(!setequal(annotSampleName , colnames(countMat))) {
    stop("SampleID-group and gct file sample names do not match. Contact the developer.")
  } else {
    countMat <- countMat[, annotSampleName, drop=FALSE]
    tpmMat <- tpmMat[, annotSampleName, drop=FALSE]
  }
  
  ## feature annotation
  genes <- readBiokitFeatureAnnotation(dir, anno=anno, verbose=verbose)
  if(!setequal(rownames(countMat), rownames(genes))) {
    warnings("FeatureNames different bewteen gct file and feature annotation")
  }
  genes <- genes[as.character(rownames(countMat)),]
  rownames(genes) <- rownames(countMat)
            
  ## remove the 3rd (group) column: 
  ## it will be added by the DGEList function below
  annotSampleGroup <- annot[, 3L]
  annot <- annot[, -3L]
  res <- DGEList(counts=countMat, samples=annot, genes=genes, 
                 group = annotSampleGroup)
  res$tpm <- tpmMat
  res$BiokitAnno <- anno
  return(res)
}
