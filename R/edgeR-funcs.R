

#' Return the size of the smallest group
#' @param obj Object
minGroupCount <- function(obj) {
  UseMethod("minGroupCount")
}

#' Return the size of the smallest group defined in the \code{DGEList} object
#' 
#' @param dgeList A \code{DGEList} object
#' @return Integer
#' 
#' @examples 
#' y <- matrix(rnbinom(12000,mu=10,size=2),ncol=6)
#' d <- DGEList(counts=y, group=rep(1:3,each=2))
#' minGroupCount(d) ## 2 
#' d2 <- DGEList(counts=y, group=rep(1:2,each=3))
#' minGroupCount(d2) ## 3
#' d3 <- DGEList(counts=y, group=rep(1:3, 1:3))
#' minGroupCount(d3) ## 1
minGroupCount.DGEList <- function(dgeList) {
  groups <- dgeList$samples$group
  if(!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(min(table(groups)))
}
#' Return the size of the smallest group defined in the \code{EdgeObject} object
minGroupCount.EdgeObject <- function(edgeObj) {
  groups <- groups(edgeObj@designContrast)
  if(!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(min(table(groups)))
}
countByGroup <- function(edgeObj) {
  groups <- groups(edgeObj@designContrast)
  if(!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(table(groups))
}
maxCountByGroup <- function(edgeObj) {
  return(max(countByGroup(edgeObj)))
}
hasNoReplicate <- function(edgeObj) {
  return(maxCountByGroup(edgeObj)<=1)
}



#' Filter lowly expressed genes by counts per million (CPM)
filterByCPM <- function(obj, ...) {
  UseMethod("filterByCPM")
}


#' Filter lowly expressed genes by CPM
#' 
#' @param obj A matrix
#' @param minCPM Numeric, the minimum CPM accepted as expressed in one sample
#' @param minCount Integer, how many samples must have CPM larger than \code{minCPM} to keep this gene?
#' 
#' @return A logical vector of the same length as the row count of the matrix. \code{TRUE} means the gene is reasonably expressed, and \code{FALSE} means the gene is lowly expressed and should be filtered (removed)
#' 
#' @examples 
#' set.seed(1887)
#' mat <- rbind(matrix(rbinom(125, 5, 0.25), nrow=25), rep(0, 5))
#' filterByCPM(mat)
filterByCPM.matrix <- function(obj, minCPM=1, minCount=1) {
  cpmRes <- cpm(obj)
  filter <- apply(cpmRes, 1, function(x) sum(x>=minCPM)>=minCount)
  return(filter)
}

#' Filter lowly expressed genes by CPM in DGEList
#' 
#' @param obj A \code{DGEList} object
#' @param minCPM Numeric, the minimum CPM accepted as expressed in one sample
#' @param minCount Integer, how many samples must have CPM larger than \code{minCPM} to keep this gene?
#' 
#' @return Another \code{DGEList} object, with lowly expressed genes removed. The original counts and gene annotation can be found in \code{counts.unfiltered} and \code{genes.unfiltered} fields, respectively. The logical vector of the filter is saved in the \code{cpmFilter} field.
#' 
#' @examples 
#' set.seed(1887)
#' mat <- rbind(matrix(rbinom(150, 5, 0.25), nrow=25), rep(0, 6))
#' d <- DGEList(mat, group=rep(1:3, each=2), genes=data.frame(Gene=sprintf("Gene%d", 1:nrow(mat))))
#' df <- filterByCPM(d)
#' 
#' nrow(df$counts.unfiltered) ## 26
#' nrow(df$counts) ## 25
filterByCPM.DGEList <- function(obj, lib.size=NULL,
                                minCPM=1,
                                minCount=minGroupCount(obj)) {
  y <- as.matrix(obj)
  genes <- obj$genes
  group <- obj$samples$group
  if (is.null(group)) 
    group <- rep_len(1L, ncol(y))
  group <- as.factor(group)
  if (is.null(lib.size)) 
    lib.size <- obj$samples$lib.size * obj$samples$norm.factors

  CPM <- cpm(y, lib.size = lib.size)
  filter <- rowSums(CPM >= minCPM) >= minCount
  res <- obj[filter,]
  res$cpmFilter <- filter
  res$counts.unfiltered <- y
  res$genes.unfiltered <- genes
  return(res)
}

#' Filter EdgeObj and remove lowly expressed genes
#' 
#' 
#' @param edgeObj An EdgeObject object
#' @param minCPM Minimal CPM value, see descriptions below
#' @param minCount Minimal count of samples in which the CPM value is no less
#' than \code{minCPM}
#' 
#' The filter is recommended by the authors of the \code{edgeR} package to
#' remove lowly expressed genes, since including them in differential gene
#' expression analysis will cause extreme differential expression fold-changes
#' of lowly and stochastically expressed genes, and increase false positive
#' rates.
#' 
#' The filter removes genes that are less expressed than 1 copy per million
#' reads (cpm) in at least \code{n} samples, where \code{n} equals the number
#' of samples in the smallest group of the design.
#' @examples
#' 
#' myFac <- gl(3,2)
#' set.seed(1234)
#' myMat <- matrix(rpois(1200,100), nrow=200, ncol=6)
#' myMat[1:3,] <- 0
#' myEdgeObj <- EdgeObject(myMat, 
#'                        DesignContrast(designMatrix=model.matrix(~myFac),
#'                         contrastMatrix=matrix(c(0,1,0), ncol=1), groups=myFac),
#'                         fData=data.frame(GeneSymbol=sprintf("Gene%d", 1:200)))
#' myFilteredEdgeObj <- filterByCPM(myEdgeObj)
#' dim(counts(myEdgeObj))
#' dim(counts(myFilteredEdgeObj))
#' ## show unfiltered count matrix
#' dim(counts(myFilteredEdgeObj, filter=FALSE))
#' 
#' @export filterByCPM.EdgeObject
filterByCPM.EdgeObject <- function(obj,
                                   minCPM=1,
                                   minCount=minGroupCount(obj)) {
  cpmRes <- cpm(obj)
  filter <- apply(cpmRes, 1, function(x) sum(x>=minCPM)>=minCount)
  newDgeList <- obj@dgeList[filter,]
  newDgeList$counts.unfiltered <- obj@dgeList$counts
  newDgeList$genes.unfiltered <- obj@dgeList$genes
  obj@dgeList <- newDgeList
  return(obj)
}
isAnyNA <- function(edgeObj) {
    any(is.na(edgeObj@dgeList$counts))
}
replaceNAwithZero <- function(edgeObj) {
  edgeObj@dgeList$counts[is.na(edgeObj@dgeList$counts)] <- 0
  return(edgeObj)
}

setMethod("normalize", "EdgeObject", function(object, method="RLE", ...) {
  object@dgeList <- calcNormFactors(object@dgeList, method=method, ...)
  return(object)
})

setMethod("cpmRNA", "EdgeObject", function(object) {
  return(cpm(object@dgeList))
})

#' Return counts in EdgeObject
#' 
#' @param object An EdgeObject
#' @param filter Logical, whether filtered matrix (by default) or unfiltered matrix should be returned
#' 
#' @seealso \code{\link{filterByCPM}}
setMethod("counts", "EdgeObject", function(object, filter=TRUE) {
  if(filter) {
    return(object@dgeList$counts)
  } else {
    return(object@dgeList$counts.unfiltered)
  }
})
setMethod("normFactors", "DGEList", function(object) {
  return(object$samples$norm.factors)
})
setMethod("normFactors", "EdgeObject", function(object) {
  return(normFactors(object@dgeList))
})


setGeneric("estimateGLMDisp", function(object) standardGeneric("estimateGLMDisp"))
setMethod("estimateGLMDisp", "EdgeObject", function(object) {
  dge <- object@dgeList
  design <- designMatrix(object@designContrast)
  ##dge <- estimateGLMCommonDisp(dge, design, verbose=FALSE)
  ##dge <- estimateGLMTrendedDisp(dge, design, verbose=FALSE)
  ##dge <- estimateGLMTagwiseDisp(dge, design)
  dge <- estimateDisp(dge, design)
  object@dgeList <- dge
  return(object)
})

setGeneric("fitGLM", function(object,...) standardGeneric("fitGLM"))
setMethod("fitGLM", "EdgeObject", function(object, ...) {
  fit <- glmFit(object@dgeList,
                design=designMatrix(object@designContrast),
                ...)
  return(fit)
})

setGeneric("testGLM", function(object, fit) standardGeneric("testGLM"))
setMethod("testGLM", c("EdgeObject", "DGEGLM"),
          function(object, fit) {
  contrasts <- contrastMatrix(object@designContrast)
  toptables <- apply(contrasts, 2, function(x) {
    lrt <- glmLRT(fit, contrast=x)
    x <- topTags(lrt, n=nrow(lrt$table))$table
    assertEdgeToptable(x)
    return(x)
  })

  return(EdgeResult(edgeObj=object,
                    dgeGLM=fit,
                    dgeTables=toptables))
})

## some useful attributes
setMethod("nrow", "EdgeResult", function(x) nrow(x@dgeList))
setMethod("ncol", "EdgeResult", function(x) ncol(x@dgeList))
dim.EdgeResult <- function(x) c(nrow(x), ncol(x))

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



dgeGML <- function(edgeResult) return(edgeResult@dgeGLM)


dgeTable <- function(edgeResult, contrast=NULL) {
    tbls <- edgeResult@dgeTables
    if(is.logical(contrast) || is.numeric(contrast)) {
        contrast <- contrastNames(edgeResult)[contrast]
    }
    if(!is.null(contrast)) {
        if(length(contrast)==0) {
            stop("No contrast selected")
        } else if (!all(contrast %in% contrastNames(edgeResult))) {
            stop("Following contrasts are not found:",
                 setdiff(contrast, contrastNames(edgeResult)))
        }
    }
    
    if(!is.null(contrast) && length(contrast)==1) {
        res <- tbls[[contrast]]
        res$Contrast <- contrast
    } else {
        if(is.null(contrast)) {
            res <- do.call(rbind, tbls)
            res$Contrast <- rep(contrastNames(edgeResult), sapply(tbls, nrow))
        } else {
            subtbls <- tbls[contrast]
            res <- do.call(rbind, subtbls)
            res$Contrast <- rep(contrast, sapply(subtbls, nrow))
        }
    }
    res <- putColsFirst(res, "Contrast")
    rownames(res) <- NULL
    return(res)
}

dgeTableList <- function(edgeResult, contrast=NULL) {
  tbls <- edgeResult@dgeTables
  if(is.null(contrast)) {
    res <- tbls
  } else {
    res <- tbls[contrast]
  }
  return(res)
}

#' Return a list of differential gene expression tables 
#' 
#' @param edgeResult An \code{EdgeResult} object
#' 
#' @return A list of \code{data.frame}s, each containing the DGEtable for one contrast.
#' 
#' @seealso \code{dgeTable} which returns one \code{data.frame} for one or more given contrasts.
dgeTables <- function(edgeResult) {
  contrs <- contrastNames(edgeResult)
  res <- lapply(contrs, function(ctr) dgeTable(edgeResult, contrast=ctr))
  names(res) <- contrs
  return(res)
}


sigFilter <- function(edgeResult) return(edgeResult@sigFilter)

sigDge <- function(edgeResult, contrast) {
  table <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
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


setMethod("show", "EdgeResult", function(object) {
  summary <- sprintf("EdgeResult object: %d genes, %d samples, %d contrasts",
                     nrow(getCounts(dgeList(object))),
                     ncol(getCounts(dgeList(object))),
                     length(object@dgeTables))
  showBCV <- "Call plotBCV() to visualize biological coefficient of variance"
  sigFilterInfo <-  sprintf("* Significant DGE filter (call updateSigFilter() to update the settings): \n%s",
                            show(sigFilter(object)))
  ER_SHOW_SEP <- paste(rep("-", 40), collapse="")
  messages <- paste(summary, showBCV,
                    ER_SHOW_SEP, sigFilterInfo, "", sep="\n")
  cat(messages)
  return(invisible(messages))
})

getGenes <- function(edgeResult) {
  rownames(edgeR::getCounts(dgeList(edgeResult)))
}

geneCount <- function(edgeResult) {
  nrow(edgeR::getCounts(dgeList(edgeResult)))
}

assertEdgeToptable <- function(x) {
  stopifnot(is.data.frame(x)
            & all(c("logFC", "logCPM", "LR", "PValue", "FDR") %in% colnames(x)))
}
isSig <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(data.frame,
       (logFC >= posLogFC(sigFilter) | logFC <= negLogFC(sigFilter)) & logCPM>=logCPM(sigFilter) & LR>=LR(sigFilter) & PValue <= pValue(sigFilter) & FDR <= FDR(sigFilter))
}
isSigPos <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(data.frame,
       logFC >= posLogFC(sigFilter) & logCPM>=logCPM(sigFilter) & LR>=LR(sigFilter) & PValue <= pValue(sigFilter) & FDR <= FDR(sigFilter))

}
isSigNeg <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(data.frame,
       logFC <= negLogFC(sigFilter) & logCPM>=logCPM(sigFilter) & LR>=LR(sigFilter) & PValue <= pValue(sigFilter) & FDR <= FDR(sigFilter))
}

## TODO: fix: add InputFeature
sigGene <- function(edgeResult, contrast, value="GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSig(tbl, sf)
  tbl[issig, value]
}
sigPosGene <- function(edgeResult, contrast, value="GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigPos(tbl, sf)
  tbl[issig, value]
}
sigNegGene <- function(edgeResult, contrast, value="GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigNeg(tbl, sf)
  tbl[issig, value]
}
sigGenes <- function(edgeResult, value="GeneID") {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigGene(edgeResult, x, value=value))
  names(res) <- cs
  return(res)
}
sigPosGenes <- function(edgeResult, value="GeneID") {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigPosGene(edgeResult, x, value=value))
  names(res) <- cs
  return(res)
}
sigNegGenes <- function(edgeResult, value="GeneID") {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigNegGene(edgeResult, x, value=value))
  names(res) <- cs
  return(res)
}
sigGeneCounts <- function(edgeResult) {
  allCount <- geneCount(edgeResult)
  posCounts <- sapply(sigPosGenes(edgeResult), ulen)
  negCounts <- sapply(sigNegGenes(edgeResult), ulen)
  total <- posCounts+negCounts
  res <- data.frame(posCount=posCounts,
                    negCount=negCounts,
                    posnegCount=posCounts+negCounts,
                    all=allCount)
  return(res)
}

sigGeneBarchart <- function(edgeResult,
                            scales=list(x=list(rot=45),
                              y=list(alternating=1, tck=c(1,0))),
                            stack=FALSE,
                            ylab="Significant DEGs",
                            col=c("positive"="orange",
                              "negative"="lightblue"),
                            logy=FALSE,
                            auto.key=list(columns=2),
                            ...) {
  counts <- sigGeneCounts(edgeResult)
  contrasts <- ribiosUtils::ofactor(contrastNames(edgeResult))
  positive <- counts$posCount
  negative <- counts$negCount
  scales$y$log <- ifelse(logy, 10, FALSE)
  lattice::barchart(positive + negative ~ contrasts,
                    stack=stack,
                    ylab=ylab,
                    scales=scales,
                    par.settings=list(superpose.polygon=list(col=col)),
                    auto.key=auto.key,
                    origin=0,
                    ...)
}

isUnsetSigFilter <- function(object) {
  isUnsetPosLogFC(object) & isUnsetNegLogFC(object) & isUnsetLogCPM(object) & isUnsetLR(object) & isUnsetPValue(object)  & isUnsetFDR(object)
}


## annotation
annotateMPS <- function(mat) {
  ampl <- attr(mat, "desc")
  stopifnot(!is.null(ampl) & all(grepl("^AMPL", ampl)))
  reporters <- mpsReporter()
  genes <- matchColumn(ampl, reporters, "Amplicon")
  rownames(genes) <- ampl
  return(genes)
}
readMPS <- function(file) {
  tbl <- read_exprs_matrix(file)
  genes <- annotateMPS(tbl)
  tbl <- data.matrix(tbl)
  return(new("FeatAnnoExprs",
             exprs=tbl,
             genes=genes))
}


#' Perform differential gene expression analysis with edgeR
#' 
#' 
#' @param edgeObj An object of \code{EdgeObject}
#' 
#' The function performs end-to-end differential gene expression (DGE) analysis
#' with common best practice using edgeR
#' @return An \code{EdgeResult} object
#' @examples
#' 
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(Identifier=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                      fData=exFdata, pData=exPdata)
#' exDgeRes <- dgeWithEdgeR(exObj)
#' dgeTable(exDgeRes)
#' 
#' @export dgeWithEdgeR
dgeWithEdgeR <- function(edgeObj) {
  edgeObj.filter <- ribiosNGS::filterByCPM(edgeObj)
  edgeObj.norm <- ribiosNGS::normalize(edgeObj.filter)
  edgeObj.disp <- estimateGLMDisp(edgeObj.norm)
  ## in case of single replicate
  ## edgeR recommendation for common dispersion: 0.4 for human study, 0.1 for well-controlled, 0.01 for tech replicates
  if(!hasCommonDisp(edgeObj.disp)) {
    warning("No common dispersion estimate available. Possible reason may be no replicates")
    warning("Common dispersion is set as 0.4. Note that the number of DEGs is sensitive to this setting")
    edgeObj.disp <- setCommonDispIfMissing(edgeObj.disp, 0.4)
  }
  edgeObj.fit <- fitGLM(edgeObj.disp)
  dgeTest <- testGLM(edgeObj.disp, edgeObj.fit)
  return(dgeTest)
}


#' Perform gene-set enrichment (GSE) analysis
#' 
#' 
#' @param edgeResult An object of the class \code{EdgeObject}
#' @param geneSets An object of the class \code{GeneSets}
#' 
#' The function performs gene-set enrichment analysis. By default,the CAMERA
#' method is applied. In case this is not successful, for instance because of
#' lack of biological replicates, the GAGE method (Generally Applicable
#' Gene-set Enrichment for pathway analysis) is applied.
#' @return An \code{EdgeGSE} object containing all information required to
#' reproduce the gene-set enrichment analysis results, as well as the
#' enrichment table. Apply \code{fullEnrichTable} to the object to extract a
#' \code{data.frame} containing results of the gene-set enrichment analysis.
#' @seealso \code{gseWithLogFCgage} and \code{gseWithCamera} are wrapped by
#' this function to perform analysis with GAGE and CAMERA, respectively.
#' \code{logFCgage} and \code{camera.EdgeResult} implements the logic, and
#' returns an object of the \code{EdgeGSE} class, which contains all relevant
#' information required to reproduce the analysis results.
#'
#' @examples
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                      fData=exFdata, pData=exPdata)
#' exDgeRes <- dgeWithEdgeR(exObj)
#' 
#' exGeneSets <- BioQC::GmtList(list(
#'     list(name="Set1", desc="set 1", genes=c("Gene1", "Gene2", "Gene3"), namespace="default"),
#'     list(name="Set2", desc="set 2", genes=c("Gene18", "Gene6", "Gene4"), namespace="default")
#' ))
#' exGse <- doGse(exDgeRes, exGeneSets)
#' fullEnrichTable(exGse)
#' 
#' exGseWithGage <- gseWithLogFCgage(exDgeRes, exGeneSets)
#' fullEnrichTable(exGseWithGage)
#' 
#' exGseWithCamera <- gseWithCamera(exDgeRes, exGeneSets)
#' fullEnrichTable(exGseWithCamera)
#' @export doGse
doGse <- function(edgeResult, geneSets) {
  res <- try(gseWithCamera(edgeResult, geneSets))
  if(class(res)=="try-error") {
    res <- gseWithLogFCgage(edgeResult, geneSets)
  }
  return(res)
}
gseWithLogFCgage <- function(edgeResult, geneSets) {
  gseRes <- logFCgage(edgeResult, geneSets)
  return(gseRes)
}
gseWithCamera <- function(edgeResult, geneSets) {
  gseRes <- camera.EdgeResult(edgeResult, geneSets)
  return(gseRes)
}


## report
writeDgeTables <- function(edgeResult, outdir=getwd()) {
  contrasts <- contrastNames(edgeResult)
  outfiles <- file.path(outdir,
                        sprintf("topTable-%s.txt", contrasts))
  tables <- lapply(contrasts, function(x) dgeTable(edgeResult, x))
  write.tableList(tables, outfiles, row.names=TRUE)
}

## write truncated DEG lists
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

groupCol <- function(edgeObj, panel="Set1") {
  fcbrewer(dispGroups(edgeObj))
}
## plotMDS
plotMDS.EdgeObject <- function(x, col, ...) {
  
  plotMDS(dgeList(x), ...)
}

#' Export dgeTest results
#' @param dgeTest A \code{EdgeResult} object
#' @param outRootDir Character string, output directory
#' @param action Character string, what happens if the output directory exists
#' 
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


#' Make static gene-level plots of an EdgeResult object
#' @param edgeResult An EdgeResult object
#' @return \code{NULL}, side effect is used
#' 
staticGeneLevelPlots <- function(edgeResult) {
  objModLogCPM <- modLogCPM(edgeResult)
  groupLabels <- dispGroups(edgeResult)
  groupCol <- fcbrewer(groupLabels)
  
  ## Dimension reduction
  ### MDS
  limma::plotMDS(edgeResult, main="MDS plot")
  
  ### PCA (using modLogCPM)
  objPca <- prcomp(t(objModLogCPM))
  objPca.data <- ribiosPlot::plotPCA(objPca, points=FALSE, text=TRUE, main="modLogCPM PCA")
  
  ### COA (using mogLogCPM)
  objCoa <- made4::ord(objModLogCPM)$or$co
  made4::plotarrays(objCoa, classvec=dispGroups(edgeResult))
  
  ## BioQC
  ### TODO: RPKM/TPM calculation needed
  ## doLog("BioQC (TODO)")
  
  ## Normalization
  ### boxplot of read counts (before and after normalization)
  normBoxplot(obj, edgeResult)
  
  ### boxplot of normalization factors
  boxplot(edgeResult, type="normFactors")
  
  ## Dispersion
  ### BCV plot
  plotBCV(edgeResult, main="BCV plot")
  
  ## Significant differentially expressed genes
  ## number of significantly differentially expressed genes
  sigGeneBarchart(edgeResult, stack=FALSE)
  
  ## volcano plot
  volcanoPlot(edgeResult, multipage=TRUE)
  
  ## plotSmear
  smearPlot(edgeResult, freeRelation=TRUE, smooth.scatter=FALSE, multipage=TRUE)
  
  ## pairs of correlations
  if(ncol(contrastMatrix(edgeResult))>1) {
    pairs(edgeResult, freeRelation=TRUE)
  }
 
  return(invisible(NULL)) 
}

#' Export static gene-level plots in PDF
#' @param edgeResult An \code{EdgeResult} object
#' @param file Character string, the PDF file name
#' 
exportStaticGeneLevelPlots <- function(edgeResult, file) {
  openFileDevice(file)
  staticGeneLevelPlots(edgeResult)
  closeFileDevice()
}
