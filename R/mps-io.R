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
#' @return A \code{DGEList} object containing counts, gene, and sample annotation
#' @examples
#' mpsnakeDir <- system.file("extdata/mpsnake-minimal-outdir", "ribiosNGS")
#' mpsDgeList <- readMpsnakeAsDGEList(mpsnakeDir)
#' 
#' ## equivalent
#' mpsnakeResDir <- file.path(mpsnakeDir, "results")
#' mpsDgeList <- readMpsnakeAsDGEList(mpsnakeResDir)
#' @importFrom ribiosIO readTable
#' @importFrom ribiosUtils assertFile isDir
#' @export
readMpsnakeAsDGEList <- function(dir) {
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

#' Extract AmpliSeq target annotation from a list of read count data.frames
#' 
#' @param readCountsList A list of read count data.frames
#' 
#' The function is used internally to extract unique target annotation from
#' multiple runs
#' 
#' @export
extractTargetAnno <- function(readCountsList) {
  .Deprecated("readMpsnakeAsDGEList")
  annos <- lapply(readCountsList, function(x) data.frame(Target=x$Target, GeneSymbol=x$Gene))
  anno <- do.call(rbind, annos)
  res <- unique(anno)
  return(res)
}

#' Merge AmpliSeq runs
#' 
#' 
#' @param readCountList A list of read count data.frames
#' @param barcodeSummaryList A list of barcode summary data.frames
#' @param runNames Character strings, run names. If \code{NULL}, a sequential
#' number will be given.
#' 
#' The function is used internally to merge several runs into one ExpressionSet
#' object.
#' 
#' @export
mergeAmpliseqRuns <- function(readCountList, barcodeSummaryList, runNames=NULL) {
  .Deprecated("readMpsnakeAsDGEList")
  if(is.null(runNames))
    runNames <- seq(along=readCountList)
  stopifnot(length(readCountList)==length(barcodeSummaryList))
  anno <- extractTargetAnno(readCountList)
  uniqTargets <- anno$Target
  
  phenoAnno <- cbind(Run=rep(runNames, sapply(barcodeSummaryList, nrow)),
                     do.call(rbind, barcodeSummaryList))
  colnames(phenoAnno) <- c("Run", "BarcodeID", "SampleName", 
                           "MappedReads", "ValidReads", "TargetsDetected")
  
  sampleInds <- lapply(seq(along=readCountList), function(i) {
    res <- match(barcodeSummaryList[[i]]$`Barcode ID`, colnames(readCountList[[i]]))
    if(any(is.na(res)))
      stop("Read-count matrix and barcode-summary matrix not matching: contact the developer")
    return(res)
  })
  featInds <- lapply(readCountList, function(x) {
    res <- match(uniqTargets, x$Target)
    if(any(is.na(res)))
      stop("Feature annotation error: contact the developer")
    return(res)
  })
  countMats <- sapply(seq(along=readCountList), function(i) readCountList[[i]][featInds[[i]], sampleInds[[i]]])
  countMat <- as.matrix(do.call(cbind, countMats))
  rownames(countMat) <- rownames(anno) <- uniqTargets
  colnames(countMat) <- rownames(phenoAnno) <- with(phenoAnno, paste(Run, BarcodeID, SampleName, sep="."))
  
  res <- new("ExpressionSet",
             exprs=countMat,
             phenoData=new("AnnotatedDataFrame", phenoAnno),
             featureData=new("AnnotatedDataFrame", anno))
  return(res)
}

#' Read AmpliSeq results into an ExpressionSet object
#' 
#' 
#' @param readCountFiles Character string vector, names of read count files
#' @param barcodeSummaryFiles Character string vector, names of barcode summary
#' files
#' @param runNames Character string vector, run names
#' 
#' This function parses read count files as well as barcode summary files, and
#' organise the data into an \code{\linkS4class{ExpressionSet}} object.
#' @examples
#' 
#' countFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("ReadCountFile-%d.xls", 1:3), package="ribiosNGS")
#' barcodeFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("BarcodeSummaryFile-%d.xls", 1:3), package="ribiosNGS")
#' exampleAmpliSeq <- readAmpliSeq(countFiles, barcodeFiles, 
#'   runNames=c("R1", "R2", "R3"))
#' 
#' @importFrom readr count_fields tokenizer_tsv read_tsv
#' @export readAmpliSeq
readAmpliSeq <- function(readCountFiles, 
                         barcodeSummaryFiles,
                         runNames=names(readCountFiles)) {
  .Deprecated("readMpsnakeAsDGEList")
  stopifnot(length(readCountFiles)==length(barcodeSummaryFiles))
  if(!is.null(runNames)) {
    stopifnot(length(readCountFiles)==length(runNames))
  } else {
    runNames <- seq(along=readCountFiles)
  }
  readCountList <- lapply(readCountFiles, function(f) {
    fields <- readr::count_fields(f, 
                                  tokenizer=tokenizer_tsv(), n_max=1)
    colTypeStr <- paste("cc", 
                        paste(rep("i", fields-2), collapse=""),
                        sep="")
    readr::read_tsv(f, col_types=colTypeStr)
  })
  barcodeSummaryList <- lapply(barcodeSummaryFiles, function(f) {
    df <- readr::read_tsv(f, col_types="ccicc")
    if(ncol(df)==4 && identical(colnames(df), c("Barcode ID", "Sample Name", "Mapped Reads", "On Target"))) {
      df$TargetsDetected <- NA
    } else if (ncol(df)==5) {
      ## okay
    } else {
      stop("barcode summary files should have either 4 or 5 columns!")
    }
    return(df)
  })
  
  res <- mergeAmpliseqRuns(readCountList, barcodeSummaryList, runNames) 
  return(res)
}


#' Detect samples that are repeated in cherry-picking runs
#' 
#' 
#' @param eset An ExpressionSet object returned by \code{\link{readAmpliSeq}}
#' @param cherryPickingRun Character string(s), name(s) of cherry picking runs
#' @note Only valid if the sample names are unique.
#' @examples
#' 
#' countFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("ReadCountFile-%d.xls", 1:3), package="ribiosNGS")
#' barcodeFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("BarcodeSummaryFile-%d.xls", 1:3), package="ribiosNGS")
#' exampleAmpliSeq <- readAmpliSeq(countFiles, barcodeFiles, 
#'   runNames=c("R1", "R2", "R3"))
#' summary(isCherryPickingRepeat(exampleAmpliSeq, "R3"))
#' 
#' @export isCherryPickingRepeat
isCherryPickingRepeat <- function(eset, cherryPickingRun) {
  .Deprecated("readMpsnakeAsDGEList")
  stopifnot(cherryPickingRun %in% eset$Run)
  dupSampleNames <- eset$SampleName[duplicated(eset$SampleName)]
  isDup <- eset$SampleName %in% dupSampleNames & !eset$Run %in% cherryPickingRun
  return(isDup)
}

#' Remove samples that are repeated in cherry-picking runs
#' 
#' 
#' @param eset An ExpressionSet object returned by \code{\link{readAmpliSeq}}
#' @param cherryPickingRun Character string(s), name(s) of cherry picking runs
#' @note Only valid if the sample names are unique.
#' @examples
#' 
#' countFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("ReadCountFile-%d.xls", 1:3), package="ribiosNGS")
#' barcodeFiles <- system.file("extdata/AmpliSeq_files/",
#'   sprintf("BarcodeSummaryFile-%d.xls", 1:3), package="ribiosNGS")
#' exampleAmpliSeq <- readAmpliSeq(countFiles, barcodeFiles, 
#'   runNames=c("R1", "R2", "R3"))
#' newAmpliSeq <- removeCherryPickingRepeat(exampleAmpliSeq, "R3")
#' 
#' @export removeCherryPickingRepeat
removeCherryPickingRepeat <- function(eset, cherryPickingRun) {
  .Deprecated("readMpsnakeAsDGEList")
  isDup <- isCherryPickingRepeat(eset, cherryPickingRun)
  return(eset[,!isDup])
}

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

