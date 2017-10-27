setMethod("EdgeObject",
          c("matrix", "DesignContrast"),
          function(object, designContrast, fData=NULL, remove.zeros=FALSE) {
              object[is.na(object)] <- 0 ## NA is replaced with zero count
              if(is.null(fData) & !is.null(rownames(object))) {
                  fData <- data.frame(InputID=rownames(object))
                  if(is.null(fData) & !is.null(attr(object, "desc"))) {
                      fData$Description <- attr(object, "desc")
                  }
              }
              dgeList <- DGEList(counts=object,
                                 group= groups(designContrast),
                                 fData=fData, remove.zeros=remove.zeros)
              new("EdgeObject",
                  dgeList=dgeList,
                  designContrast=designContrast)
          })
setMethod("EdgeObject",
          c("FeatAnnoExprs", "DesignContrast"),
          function(object, designContrast, remove.zeros=FALSE) {
            dgeList <- DGEList(counts=object@exprs,
                               group=groups(designContrast),
                               fData=object@fData, remove.zeros=remove.zeros)
            new("EdgeObject",
                dgeList=dgeList,
                designContrast=designContrast)
          })
minGroupCount <- function(edgeObj) {
  groups <- groups(edgeObj@designContrast)
  return(min(table(groups)))
}
countByGroup <- function(edgeObj) {
  groups <- groups(edgeObj@designContrast)
  return(table(groups))
}
maxCountByGroup <- function(edgeObj) {
  return(max(countByGroup(edgeObj)))
}
hasNoReplicate <- function(edgeObj) {
  return(maxCountByGroup(edgeObj)<=1)
}
filterByCPM <- function(edgeObj,
                        minCPM=1,
                        minCount=minGroupCount(edgeObj)) {
  cpm <- cpm(edgeObj)
  filter <- apply(cpm, 1, function(x) sum(x>=minCPM)>=minCount)
  edgeObj@dgeList <- edgeObj@dgeList[filter,]
  return(edgeObj)
}
isAnyNA <- function(edgeObj) {
    any(is.na(edgeObj@dgeList$counts))
}
replaceNAwithZero <- function(edgeObj) {
  edgeObj@dgeList$counts[is.na(edgeObj@dgeList$counts)] <- 0
  return(edgeObj)
}

setMethod("normalize", "EdgeObject", function(object, ...) {
  object@dgeList <- calcNormFactors(object@dgeList, ...)
  return(object)
})

setMethod("cpmRNA", "EdgeObject", function(object) {
  return(cpm(object@dgeList))
})
setMethod("counts", "EdgeObject", function(object) {
  return(object@dgeList$counts)
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
dgeTables <- function(edgeResult) dgeTable(edgeResult, contrast=NULL)


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
sigGene <- function(edgeResult, contrast) {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSig(tbl, sf)
  tbl$GeneID[issig]
}
sigPosGene <- function(edgeResult, contrast) {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigPos(tbl, sf)
  tbl$GeneID[issig]
}
sigNegGene <- function(edgeResult, contrast) {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigNeg(tbl, sf)
  tbl$GeneID[issig]
}
sigGenes <- function(edgeResult) {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigGene(edgeResult, x))
  names(res) <- cs
  return(res)
}
sigPosGenes <- function(edgeResult) {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigPosGene(edgeResult, x))
  names(res) <- cs
  return(res)
}
sigNegGenes <- function(edgeResult) {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x) sigNegGene(edgeResult, x))
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
##annotateDataFrame <- function(df, annotation, key) {
##  stopifnot(is.data.frame(annotation))
##  if(!missing(key)) {
##    keys <- annotation[, key]
##  } else {
##    keys <- rownames(annotation)
##  }
##  ind <- match(rownames(df), keys)
##  df.anno <- annotation[ind,]
##  if(any(!colnames(df.anno) %in% colnames(df))) {
##    res <- cbind(df.anno, df)
##  } else {
##    res <- df
##  }
##  rownames(res) <- rownames(df)
##  return(res)
##}
##annotateGenes <- function(edgeResult, annotation, key) {
##  dt <- dgeTables(edgeResult)
##  annodt <- lapply(dt, function(x) annotateDataFrame(x, annotation, key))
##  edgeResult@dgeTables <- annodt
##  return(edgeResult)
##}




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
               writeMatrix(degs$neg,
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

## sniff annotation

