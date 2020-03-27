#' Extract AmpliSeq target annotation from a list of read count data.frames
#' 
#' 
#' @param readCountsList A list of read count data.frames
#' 
#' The function is used internally to extract unique target annotation from
#' multiple runs
extractTargetAnno <- function(readCountsList) {
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
mergeAmpliseqRuns <- function(readCountList, barcodeSummaryList, runNames=NULL) {
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
#' @export readAmpliSeq
readAmpliSeq <- function(readCountFiles, 
                         barcodeSummaryFiles,
                         runNames=names(readCountFiles)) {
  stopifnot(length(readCountFiles)==length(barcodeSummaryFiles))
  if(!is.null(runNames)) {
    stopifnot(length(readCountFiles)==length(runNames))
  } else {
    runNames <- seq(along=readCountFileList)
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
#'
readMolPhenCoverageGct <- function(file) {
  mat <- ribiosIO::read_gct_matrix(file)
  fData <- parseMolPhenFeat(mat)
  res <- list(coverage=mat, genes=fData)
  return(res)
}

toString.data.frame = function (object, ..., digits=NULL, quote=FALSE, right=TRUE, row.names=TRUE) {
  nRows = length(row.names(object));
  if (length(object)==0) {
    return(paste(
      sprintf(ngettext(nRows, "data frame with 0 columns and %d row", "data frame with 0 columns and %d rows")
              , nRows)
      , "\\n", sep = "")
    ); 
  } else if (nRows==0) {
    return(gettext("<0 rows> (or 0-length row.names)\\n")); 
  } else {
    # get text-formatted version of the data.frame
    m = as.matrix(format.data.frame(object, digits=digits, na.encode=FALSE)); 
    # define row-names (if required)
    if (isTRUE(row.names)) {
      rowNames = dimnames(object)[[1]];
      if(is.null(rowNames)) { 
        # no row header available -> use row numbers
        rowNames = as.character(1:NROW(m));
      } 
      # add empty header (used with column headers)
      rowNames = c("", rowNames);
    }
    # add column headers
    m = rbind(dimnames(m)[[2]], m);
    # add row headers
    m = cbind(rowNames, m);
    # max-length per-column
    maxLen = apply(apply(m, c(1,2), stringr::str_length), 2, max, na.rm=TRUE);
    
    # add right padding
    ##  t is needed because "If each call to FUN returns a vector of length n, then apply returns an array of dimension c(n, dim(X)[MARGIN])"
    m = t(apply(m, 1, stringr::str_pad, width=maxLen, side="right"));
    m = t(apply(m, 1, stringr::str_pad, width=maxLen+3, side="left"));
    # merge columns
    m = apply(m, 1, paste, collapse="");
    # merge rows (and return)
    return(paste(m, collapse="\n"));
  }
}

#' Read Illumina MolPhen sample sheet from XLS files
#' @param file A XLS/XLSX file containing in the first sheet
#'  the sample sheet of a molecular phenotyping experiment
#' @return A \code{data.frame} annotating the samples
#' 
read_illumina_sampleSheet_xls <- function(file) {
   content <- readxl::read_excel(file, col_names=FALSE, 
                                 .name_repair = "minimal")
   tmpf <- tempfile()
   ribiosIO::writeMatrix(content, tmpf, row.names = FALSE)
   res <- ribiosIO::read_illumina_sampleSheet(tmpf,
                                              sep="\t")
   return(res)
}
