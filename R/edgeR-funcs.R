ESF_POSLOGFC_DEFAULT <- 0
ESF_NEGLOGFC_DEFAULT <- 0
ESF_LOGCPM_DEFAULT <- -Inf
ESF_LR_DEFAULT <- 0
ESF_PVALUE_DEFAULT <- 1
ESF_FDR_DEFAULT <- 1

setClass("EdgeSigFilter",
         representation=list("posLogFC"="numeric",
           "negLogFC"="numeric",
           "logCPM"="numeric",
           "LR"="numeric",
           "pValue"="numeric",
           "FDR"="numeric"),
         prototype=list(posLogFC=ESF_POSLOGFC_DEFAULT,
           negLogFC=ESF_NEGLOGFC_DEFAULT ,
           logCPM=ESF_LOGCPM_DEFAULT,
           LR=ESF_LR_DEFAULT,
           pValue=ESF_PVALUE_DEFAULT,
           FDR=ESF_FDR_DEFAULT),
         validity=function(object) {
           stopifnot(validPosLogFC <- object@posLogFC >= 0)
           stopifnot(validNegLogFC <- object@negLogFC <= 0)
           stopifnot(validLR <- object@LR>=0)
           stopifnot(validPvalue <- object@pValue >= 0 & object@pValue <= 1)
           stopifnot(validFDR <- object@FDR >= 0 & object@FDR <= 1)
           return(validPosLogFC & validNegLogFC & validLR & validPvalue & validFDR)
         })
posLogFC <- function(edgeSigFilter) edgeSigFilter@posLogFC
negLogFC <- function(edgeSigFilter) edgeSigFilter@negLogFC
logCPM <- function(edgeSigFilter) edgeSigFilter@logCPM
LR <- function(edgeSigFilter) edgeSigFilter@LR
pValue <- function(edgeSigFilter) edgeSigFilter@pValue
FDR <- function(edgeSigFilter) edgeSigFilter@FDR
isUnsetPosLogFC <- function(edgeSigFilter) posLogFC(edgeSigFilter)==ESF_POSLOGFC_DEFAULT
isUnsetNegLogFC <- function(edgeSigFilter) negLogFC(edgeSigFilter)==ESF_NEGLOGFC_DEFAULT
isUnsetLogCPM <- function(edgeSigFilter) logCPM(edgeSigFilter)==ESF_LOGCPM_DEFAULT
isUnsetLR <- function(edgeSigFilter) LR(edgeSigFilter)==ESF_LR_DEFAULT
isUnsetPValue <- function(edgeSigFilter) pValue(edgeSigFilter)==ESF_PVALUE_DEFAULT
isUnsetFDR <- function(edgeSigFilter) FDR(edgeSigFilter)==ESF_FDR_DEFAULT

setMethod("show", "EdgeSigFilter", function(object) {
  title <- "Edge Significantly Expressed Genes Filter"
  msgs <- c()
  if(!isUnsetPosLogFC(object))
    msgs <- c(msgs,
                  sprintf("posLogFC filter set: logFC>=%f", posLogFC(object)))
  if(!isUnsetNegLogFC(object))
    msgs <- c(msgs,
                  sprintf("negLogFC filter set: logFC<=%f", negLogFC(object)))
  if(!isUnsetLogCPM(object))
    msgs <- c(msgs,
                  sprintf("logCPM filter set: logCPM>=%f", logCPM(object)))                  
  if(!isUnsetLR(object))
    msgs <- c(msgs,
                 sprintf("LR filter set: LR>=%f", LR(object)))
  if(!isUnsetPValue(object))
    msgs <- c(msgs,
                  sprintf("pValue filter set: pValue<=%f", pValue(object)))
  if(!isUnsetFDR(object))
    msgs <- c(msgs,
                  sprintf("FDR filter set: FDR<=%f", FDR(object)))
  if(length(msgs)==0L)
    msgs <- c(msgs, "No active filter set")
  messages <- paste(paste(title,
                          paste("*", msgs, collapse="\n"),
                          sep="\n"), "\n", sep="")
  
  cat(messages)
  return(invisible(messages))
})

`posLogFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@posLogFC <- value
  return(edgeSigDegFilter)
}
`negLogFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@negLogFC <- value
  return(edgeSigDegFilter)
}
`logFC<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@posLogFC <- abs(value)
  edgeSigDegFilter@negLogFC <- -abs(value)
  return(edgeSigDegFilter)
}
`logCPM<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@logCPM <- value
  return(edgeSigDegFilter)
}
`LR<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@LR <- value
  return(edgeSigDegFilter)
}
`pValue<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@pValue <- value
  return(edgeSigDegFilter)
}
`FDR<-` <- function(edgeSigDegFilter, value) {
  edgeSigDegFilter@FDR <- value
  return(edgeSigDegFilter)
}
update.EdgeSigFilter <- function(object, logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  if(!missing(logFC)) logFC(object) <- logFC
  if(!missing(posLogFC)) posLogFC(object) <- posLogFC
  if(!missing(negLogFC)) negLogFC(object) <- negLogFC
  if(!missing(logCPM)) logCPM(object) <- logCPM
  if(!missing(LR)) LR(object) <- LR
  if(!missing(pValue)) pValue(object) <- pValue
  if(!missing(FDR)) FDR(object) <- FDR
  validObject(object)
  return(object)
}
EdgeSigFilter <- function(logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  object <- new("EdgeSigFilter")
  object <- update(object, logFC=logFC, posLogFC=posLogFC, negLogFC=negLogFC,
                   logCPM=logCPM, LR=LR, pValue=pValue, FDR=FDR)
  return(object)
}


isUnsetSigFilter <- function(object) {
  isUnsetPosLogFC(object) & isUnsetNegLogFC(object) & isUnsetLogCPM(object) & isUnsetLR(object) & isUnsetPValue(object)  & isUnsetFDR(object)
}

ER_TCRATIO_DEFAULT <- Inf
ER_TRENDED_DEFAULT <- Inf
ER_AVELOGCPM_DEFAULT <- -Inf
ER_SIGFILTER_DEFAULT <- EdgeSigFilter(logFC=0.25, FDR=0.10)

setClass("EdgeResult",
         representation=list("dgeList"="DGEList",
           "dgeGLM"="DGEGLM",
           "contrasts"="matrix",
           "dgeTables"="list",
           "isFilteredGene"="logical",
           "tcRatioCutoff"="numeric",
           "trendedCutoff"="numeric",
           "aveLogCPMCutoff"="numeric",
           "sigFilter"="EdgeSigFilter"),
         prototype=list(tcRatioCutoff=ER_TCRATIO_DEFAULT,
           trendedCutoff=ER_TRENDED_DEFAULT,
           aveLogCPMCutoff=ER_AVELOGCPM_DEFAULT,
           sigFilter=ER_SIGFILTER_DEFAULT))

EdgeResult <- function(dgeList, dgeGLM, contrasts, dgeTables) {
  new("EdgeResult",
      dgeList=dgeList, dgeGLM=dgeGLM, contrasts=contrasts, dgeTables=dgeTables,
      isFilteredGene=rep(TRUE, nrow(getCounts(dgeList))))
}
dgeList <- function(edgeResult) return(edgeResult@dgeList)
dgeGML <- function(edgeResult) return(edgeResult@dgeGLM)
contrastMatrix <- function(edgeResult)  {return(edgeResult@contrasts)}
dgeTables <- function(edgeResult) return(edgeResult@dgeTables)
dgeFilteredTables <- function(edgeResult) {
  filtered <- filteredGenes(edgeResult)
  tables <- dgeTables(edgeResult)
  lapply(tables, function(x) x[rownames(x) %in% filtered,])
}
sigFilter <- function(edgeResult) return(edgeResult@sigFilter)
isFilteredGene <- function(edgeResult) return(edgeResult@isFilteredGene)
tcRatioCutoff <- function(edgeResult) {return(edgeResult@tcRatioCutoff)}
isUnsetTcRatioCutoff <- function(edgeResult) {tcRatioCutoff(edgeResult) == ER_TCRATIO_DEFAULT}
trendedCutoff <- function(edgeResult) {return(edgeResult@trendedCutoff)}
isUnsetTrendedCutoff <- function(edgeResult) {trendedCutoff(edgeResult) == ER_TRENDED_DEFAULT}
aveLogCPMCutoff <- function(edgeResult) {return(edgeResult@aveLogCPMCutoff)}
isUnsetAveLogCPMCutoff <- function(edgeResult) {return(aveLogCPMCutoff(edgeResult) == ER_AVELOGCPM_DEFAULT)}


contrastNames <- function(edgeResult) {return(names(edgeResult@dgeTables))}
designMatrix <- function(edgeResult)  {return(dgeGML(edgeResult)$design)}

`isFilteredGene<-` <- function(edgeResult, value) {
  edgeResult@isFilteredGene <- value
  return(edgeResult)
}
`tcRatioCutoff<-` <- function(edgeResult, value) {
  stopifnot(length(value)==1)
  edgeResult@tcRatioCutoff <- value
  return(updateIsFilterGene(edgeResult))
}
`trendedCutoff<-` <- function(edgeResult, value) {
  stopifnot(length(value)==1)
  edgeResult@trendedCutoff <- value
  return(updateIsFilterGene(edgeResult))
}
`aveLogCPMCutoff<-` <- function(edgeResult, value) {
  stopifnot(length(value)==1)
  edgeResult@aveLogCPMCutoff <- value
  return(updateIsFilterGene(edgeResult))
}
unsetTcRatioCutoff <- function(edgeResult) {
  tcRatioCutoff(edgeResult) <- ER_TCRATIO_DEFAULT
  return(edgeResult)
}
unsetTrendedCutoff <- function(edgeResult) {
  trendedCutoff(edgeResult) <- ER_TRENDED_DEFAULT
  return(edgeResult)
}
unsetAveLogCPMCutoff <- function(edgeResult) {
  aveLogCPMCutoff(edgeResult) <- ER_AVELOGCPM_DEFAULT
  return(edgeResult)
}

`sigFilter<-` <- function(edgeResult, value) {
  edgeResult@sigFilter <- value
  return(edgeResult)
}
updateSigFilter <- function(edgeResult, logFC, posLogFC, negLogFC, logCPM, LR, pValue, FDR) {
  sf <- sigFilter(edgeResult)
  sf <- update(sf,
               logFC=logFC, posLogFC=posLogFC, negLogFC=negLogFC, logCPM=logCPM, LR=LR, pValue=pValue, FDR=FDR)
  sigFilter(edgeResult) <- sf
  return(edgeResult)
}

ER_SHOW_SEP <- paste(rep("-", 40), collapse="")
setMethod("show", "EdgeResult", function(object) {
  summary <- sprintf("EdgeResult object: %d genes, %d samples, %d contrasts",
                     nrow(getCounts(dgeList(edgeRes))),
                     ncol(getCounts(dgeList(edgeRes))),
                     length(object@dgeTables))
  showBCV <- "Call plotBCV() to visualize biological coefficient of variance"
  tcrInfo <- ifelse(!isUnsetTcRatioCutoff(object),
                       sprintf("* Trended/Common BCV ratio cutoff=%f", tcRatioCutoff(object)),
                       "* Call tcRatio() to set the trended/common BCV ratio threshold")
  trendedInfo <- ifelse(!isUnsetTrendedCutoff(object),
                        sprintf("* Trended BCV ratio cutoff=%f", trendedCutoff(object)),
                        "* Call trended() to set the trended BCV ratio threshold")
  aveLogCPMInfo <- ifelse(!isUnsetAveLogCPMCutoff(object),
                        sprintf("* Average LogCPM (copies per million) cutoff=%f", aveLogCPMCutoff(object)),
                        "* Call aveLogCPM() to set the average LogCPM threshold")
  filterInfo <- sprintf("Genes passing current filters: %d", sum(isFilteredGene(object)))
  sigFilterInfo <-  sprintf("* Significant DGE filter (call updateSigFilter() to update the settings): \n%s",
                            show(sigFilter(object)))
  messages <- paste(summary, showBCV,
                    ER_SHOW_SEP, tcrInfo, trendedInfo, aveLogCPMInfo, filterInfo,
                    ER_SHOW_SEP, sigFilterInfo, "", sep="\n")
  cat(messages)
  return(invisible(messages))
})



naOrSqrt <- function(x) {
  if(is.null(x)) { return (NA)}
  return(sqrt(x))
}
setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))
setMethod("commonBCV", "DGEList", function(x) {
  naOrSqrt(x$common.dispersion)
})
setMethod("tagwiseBCV", "DGEList", function(x) {
  naOrSqrt(x$tagwise.dispersion)
})
setMethod("trendedBCV", "DGEList", function(x) {
  naOrSqrt(x$trended.dispersion)
})
setMethod("commonBCV", "EdgeResult", function(x)  {
  commonBCV(dgeList(x))
})
setMethod("tagwiseBCV", "EdgeResult", function(x)  {
  tagwiseBCV(dgeList(x))
})
setMethod("trendedBCV", "EdgeResult", function(x)  {
  trendedBCV(dgeList(x))
})
setGeneric("BCV", function(x) standardGeneric("BCV"))
setMethod("BCV", "DGEList", function(x) {
  A <- x$AveLogCPM
  if(is.null(getDispersion(x))) stop("No dispersion available")
  res <- data.frame(aveLogCPM=A,
                    commonBCV = commonBCV(x),
                    tagwiseBCV=tagwiseBCV(x),
                    trendedBCV=trendedBCV(x))
  rownames(res) <- rownames(x$counts)
  return(res)
})

setMethod("BCV", "EdgeResult", function(x) {
  BCV(dgeList(x))
})


setMethod("aveLogCPM", "EdgeResult", function(y, ...) {
  return(aveLogCPM(dgeList(y)))
})

tcRatio <- function(edgeResult) {
  trendedBCV(edgeResult)/commonBCV(edgeResult)
}

isTcRatioFiltered <- function(edgeResult) {
  tcRatio(edgeResult)<=tcRatioCutoff(edgeResult)
}

isTrendedFiltered <- function(edgeResult) {
  trendedBCV(edgeResult)<=trendedCutoff(edgeResult)
}

isAveLogCPMFiltered <- function(edgeResult) {
  aveLogCPM(edgeResult)>=aveLogCPMCutoff(edgeResult)
}

tcRatioAveLog <- function(edgeResult) {
  isFilter <- isTcRatioFiltered(edgeResult)
  aveExp <- aveLogCPM(edgeResult)
  min(aveExp[isFilter], na.rm=TRUE)
}

getGenes <- function(edgeResult) rownames(getCounts(dgeList(edgeResult)))
emptyFilter <- function(edgeResult)
  rep(TRUE, nrow(getCounts(dgeList(edgeResult))))
updateIsFilterGene <- function(edgeResult) {
  gf <- emptyFilter(edgeResult)
  gf <- gf & isTcRatioFiltered(edgeResult)
  gf <- gf & isTrendedFiltered(edgeResult)
  gf <- gf & isAveLogCPMFiltered(edgeResult)
  isFilteredGene(edgeResult) <- gf
  return(edgeResult)
}

filteredGenes <- function(edgeResult) {
  getGenes(edgeResult)[isFilteredGene(edgeResult)]
}

setGeneric("plotBCV", function(x, ...) standardGenerics("plotBCV"))
setMethod("plotBCV", "DGEList", function(x, ...) {
  edgeR::plotBCV(x, ...)
})
setMethod("plotBCV", "EdgeResult", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
  isFilter <- isFilteredGene(x)
  if(!all(isFilter)) {
    points(aveLogCPM(x)[!isFilter], tagwiseBCV(x)[!isFilter], col="lightgray", pch=16, cex=0.2)
  }
  if(!isUnsetTcRatioOff(x)) {
    aveLogCPM.thr <- tcRatioAveLog(x)
    abline(v=aveLogCPM.thr, col="black", lty=1)
    texty <- (par("usr")[4]-par("usr")[3])*0.9+par("usr")[3]
    text(aveLogCPM.thr,texty, sprintf("Trended/Common BCV<=%.1f (%d genes)",
                                      tcRatio(x), sum(isTcRatioFiltered(x))),
         adj=-0.01)
  }
})


#' Build DGEList object ready for generalized linear model fitting
#'
#' @description
#' The function combines the step of object initialization, library size calculation, and dispersion estimation 
#'
#' @note
#' robust=TRUE is only valid for edgeR version >= 3.6.0
#'
#' @param x a numerical matrix or any object that can be transformed as such
#' @param design design matrix (subject to dimensionality check)
#' @param robust Whether robust estimation (Zhou \emph{et al.}, NAR 2014) should be used
#' @return A DGEList object with dispersions estimated, ready for \code{glmFit}.
#' @seealso \code{glmFit} to fit a generalized linear model
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1))
#' edgeObj <- edgeBuild(expression, design)
#' @export
edgeBuild <- function(x, design, robust=FALSE) {
  x <- as.matrix(x)
  dge <- DGEList(counts=x)
  dge <- calcNormFactors(dge, method="TMM")
  if(robust) {
    dge <- estimateGLMRobustDisp(dge, design)
  } else {
    dge <- estimateGLMCommonDisp(dge, design, verbose=FALSE)
    dge <- estimateGLMTrendedDisp(dge, design, verbose=FALSE)
    dge <- estimateGLMTagwiseDisp(dge, design)
  }
  return(dge)
}

#' Test differentially expressed genes using edgeR
#'
#' @description
#' The function tests differentially expressed genes using generalized linear model likelihood-ratio tests
#' A single contrast vector is allowed
#'
#' @keywords internal
#' @param fit An \code{DGEGLM} object produced by \code{glmFit}
#' @param contrast a vector of contrasts
#' @return A data frame containing information about differential expression
#' @seealso \code{\link{glmFit}} to produce the input object, \code{\link{edgeTest}} to run the test for a contrast matrix.
#'
#' @note
#' The function checks that the resulting table must contain the following columns: logFC, logCPM, LR, PValue, FDR
#'
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1))
#' edgeObj <- edgeBuild(expression, design)
#' edgeFit <- glmFit(edgeObj, design)
#' \dontrun{
#' edgeFitTest <- edgeTestContrast(edgeFit, contrast=c(0,1))
#' }
edgeTestContrast <- function(fit, contrast) {
  stopifnot(is.vector(contrast))
  lrt <- glmLRT(fit, contrast=contrast)
  x <- topTags(lrt, n=nrow(lrt$table))$table
  stopifnot(all(c("logFC", "logCPM", "LR", "PValue", "FDR") %in% colnames(x)))
  return(x)
}

#' Test differentially expressed genes using edgeR
#'
#' @description
#' The function tests differentially expressed genes using generalized linear model likelihood-ratio tests
#' Contrasts in the matrix are sequentially fitted
#'
#' @param fit An \code{DGEGLM} object produced by \code{glmFit}
#' @param contrasts a contrast matrix. If a vector is given, it is converted into a column vector.
#' @return A list of data frames containing information about differential expression
#' @seealso \code{\link{edgeTestContrast}} to test one contrast vector
#' 
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1), y=c(-1,1,-1,1))
#' edgeObj <- edgeBuild(expression, design)
#' edgeFit <- glmFit(edgeObj, design)
#' contrast1 <- c(0,1, 0)
#' contrast2 <- c(0,1, -1)
#' contrast3 <- cbind(C1=contrast1, C2=contrast2)
#' edgeTest1 <- edgeTest(edgeFit, contrast1)
#' edgeTest2 <- edgeTest(edgeFit, contrast2)
#' edgeTest3 <- edgeTest(edgeFit, contrast3)
#' stopifnot(identical(names(edgeTest3), c("C1", "C2")))
#' @export
edgeTest <- function(fit,  contrasts) {
  contrasts <- as.matrix(contrasts)
  assertContrast(fit$design, contrasts)
  toptables <- apply(contrasts, 2, function(x)
                    edgeTestContrast(fit, x))
  return(toptables)
}

#' Identify differentially expressed genes using generalized linear models implemented by edgeR
#'
#' @description
#' The function provides a simple interface to run differential gene analysis using edgeR using default settings
#' Users desiring higher flexibility may call functions individually
#' It is user's responsibility to understand the assumptions behind the default settings and to judge whether they are suitable for the data
#' @param x A matrix of read counts, or any objects that can be converted to a matrix
#' @param design Design matrix (subject to dimensionality check)
#' @param contrasts Contrast matrix (subject to dimensionality check)
#' @param robust Logical, whether robust dispersion estimation should be used or not
#'
#' @return A list of data frames reporting differntial gene expression
#' @seealso
#' \code{\link{edgeBuild}} to build a DGEList object and to estimate dispersions
#' \code{\link{glmFit}} to fit a DGEList object against the design matrix
#' \code{\link{edgeTest}} to derive lists of differentially expressed genes
#'
#' #' @note
#' robust=TRUE is only valid for edgeR version >= 3.6.0
#'
#' @examples
#' expression <- matrix(rnbinom(10000,mu=5,size=2),ncol=4)
#' design <- cbind(baseline=c(1,1,1,1), x=c(1,1,-1,-1), y=c(-1,1,-1,1))
#' contrast <- cbind(C1=c(0,1,0), C2=c(0,1,-1))
#' edgeDiff <- edgeRun(expression, design, contrast, robust=FALSE)
#' @export
edgeRun <- function(x, design, contrasts, robust=FALSE) {
  dge <- edgeBuild(x, design, robust=robust)
  fit <- glmFit(dge, design)
  contrasts <- as.matrix(contrasts)
  test <- edgeTest(fit, contrasts)
  return(EdgeResult(dge, fit, contrasts, test))
}

## zhangj83 2014-10-31
## the following two functions are going to be factored out
##edgeR.DGE <- function(path=".",
##                      targets.file="Targets.txt",
##                      normFactor.method="RLE") {
##  target.file.full <- file.path(path,targets.file)
##  if(!file.exists(target.file.full))
##    stop("target.file not found at ", target.file.full)
##  targets <- read.delim(file=target.file.full,
##                        stringsAsFactors=FALSE)
##  data <- readDGE(targets, path=path, comment.char="#", head=FALSE)
##  d <- calcNormFactors(data, method=normFactor.method)
##  d <- estimateCommonDisp(d)
##  d
##}
##
##edgeR.sumCountByGroup <- function(data,
##                                  group.colName="group"){
##  if(!group.colName %in% colnames(data$sample))
##    stop("'group.colName' must be one of the following:\n",
##         paste(colnames(data$sample), collapse=","))
##  do.call(cbind,
##          tapply(1:ncol(data$counts),
##                 data$sample[,group.colName],
##                 function(x) rowSums(data$counts[,x,drop=FALSE])))
##}
