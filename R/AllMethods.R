setMethod("dgeList", "EdgeObject", function(object) return(object@dgeList))
setMethod("dgeList", "EdgeResult", function(object) return(object@dgeList))

setMethod("designMatrix", "EdgeObject", function(object) designMatrix(object@designContrast))
setMethod("designMatrix", "EdgeResult", function(object) designMatrix(object@designContrast))

setMethod("contrastMatrix", "EdgeObject", function(object) contrastMatrix(object@designContrast))
setMethod("contrastMatrix", "EdgeResult", function(object) contrastMatrix(object@designContrast))

setMethod("contrastNames", "EdgeObject", function(object) colnames(contrastMatrix(object)))
setMethod("contrastNames", "EdgeResult", function(object) colnames(contrastMatrix(object)))
setMethod("contrastNames",
          "EdgeGSE",
          function(object) contrastNames(as(object, "EdgeObject")))

setMethod("designMatrix<-", c("EdgeObject", "matrix"), function(object, value) {
  object@designContrast@design <- value
  return(object)
})
setMethod("contrastMatrix<-", c("EdgeObject", "matrix"), function(object, value) {
  object@designContrast@contrasts <- value
  return(object)
})

## nContrast and contrastSampleIndices
setMethod("nContrast", "EdgeResult", function(object) {nContrast(object@designContrast)})
setMethod("contrastSampleIndices", c("EdgeResult", "character"), function(object, contrast) {
              contrastSampleIndices(object@designContrast, contrast)
          })
setMethod("contrastSampleIndices", c("EdgeResult", "integer"), function(object, contrast) {
              contrastSampleIndices(object@designContrast, contrast)
          })



naOrSqrt <- function(x) {
  if(is.null(x)) { return (NA)}
  return(sqrt(x))
}
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

## edgeR::aveLogCPM is a S3 method
aveLogCPM.EdgeResult <- function(y,...) {
    return(aveLogCPM(dgeList(y)))
}
## setMethod("aveLogCPM", "EdgeResult", function(y, ...) {
##  return(aveLogCPM(dgeList(y)))
##})

## plotBCV
setMethod("plotBCV", "DGEList", function(x, ...) {
  edgeR::plotBCV(x, ...)
})
setMethod("plotBCV", "EdgeObject", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})
setMethod("plotBCV", "EdgeResult", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})

## groups
setMethod("groups", "EdgeObject", function(object) {
  return(groups(object@designContrast))
})
setMethod("dispGroups", "EdgeObject", function(object) {
  return(dispGroups(object@designContrast))
})

## boxplot
normFactorBoxplot <- function(edgeObj,...) {
  strwidth.inch <- strwidth(dispGroups(edgeObj),
                            units="inch")
  op <- par(mai=c(quantile(strwidth.inch, 0.95)+0.5,
              par("mai")[2:4]*c(1, 0.8, 0.8)))
  on.exit(par(op))
  boxplot(normFactors(edgeObj)~dispGroups(edgeObj), las=3,...)
  abline(h=1, col="lightgray")
}
modLogCPMBoxplot <- function(edgeObj, ...) {
  sample.names <- colnames(dgeList(edgeObj)$counts)
  modLogCPM <- modLogCPM(edgeObj)
  strwidth.inch <- strwidth(sample.names,
                            units="inch")
  op <- par(mai=c(quantile(strwidth.inch, 0.95)+0.5,
              par("mai")[2:4]*c(1, 0.8, 0.8)))
  on.exit(par(op))
  boxplot(modLogCPM, las=3,...)
  avg <- median(modLogCPM, na.rm=TRUE)
  abline(h=avg, col="lightgray")
}

nonNull <- function(x, val) return(ifelse(is.null(x), val, x))
setMethod("boxplot", "EdgeObject",
          function(x, type=c("normFactors","modLogCPM"), xlab="", ylab=NULL, main="", ...) {
            type <- match.arg(type)
            if(type=="normFactors") {
              ylab <- nonNull(ylab, "Normalization factors")
              normFactorBoxplot(x, xlab=xlab, ylab=ylab, main=main, ...)
            } else if (type=="modLogCPM") {
              ylab <- nonNull(ylab, "log2(moderated cpm)")
              modLogCPMBoxplot(x, xlab=xlab, ylab=ylab, main=main,...)
            } 
          })

normBoxplot <- function(before.norm, after.norm, ...) {
  op <- par(mfrow=c(1,2))
  on.exit(par(op))
  boxplot(before.norm, type="modLogCPM", main="Before norm.", ...)
  boxplot(after.norm, type="modLogCPM", main="After norm.", ...)
}

## volcanoPlot
quantileRange <- function(x, outlier=0.01, symmetric=TRUE) {
  quants <- c(outlier/2, 1-outlier/2)
  qts <- quantile(x, quants, na.rm=TRUE)
  if(symmetric) {
    mm <- max(abs(qts))
    res <- c(-mm, mm)
  } else {
    res <- qts
  }
  return(res)
}
setMethod("volcanoPlot", "EdgeResult",
          function(object, contrast=NULL,
                   freeRelation=FALSE,
                   colramp=ribiosPlot::heat,
                   multipage=FALSE,
                   ...) {
  tables <- dgeTableList(object, contrast)
  logFCs <- unlist(sapply(tables, function(x) x$logFC))
  ps <- unlist(sapply(tables, function(x) x$PValue))
  if(!freeRelation) {
    logFC.range <- quantileRange(logFCs, outlier=0.01, symmetric=TRUE)
    pValue.range <- quantileRange(ps, outlier=0.01, symmetric=FALSE)
    xlim <- logFC.range
    ylim <- c(0, max(-log10(pValue.range)))
  }

  if(!multipage) {
    op <- ribiosPlot::compactPar()
    on.exit(par(op))
    op2 <- par(mfrow=grDevices::n2mfrow(length(tables)))
  }
  
  for(i in seq(along=tables)) {
    if(freeRelation) {
      with(tables[[i]], smoothScatter(-log10(PValue)~logFC,
                                      colramp=colramp,
                                      main=names(tables[i]),...))
    }  else {
      with(tables[[i]], smoothScatter(-log10(PValue)~logFC,
                                      colramp=colramp,
                                      main=names(tables[i]),
                                      xlim=xlim, ylim=ylim, ...))
    }
    abline(h=0, col="lightgray")
    abline(v=0, col="lightgray")
  }

  if(!multipage) {
    par(op2)
  }
})

customSmearPlot <- function(tbl, main, 
                              xlab = "Average logCPM", 
                              ylab = "logFC", pch = 19, cex = 0.2, smearWidth = 0.5, panel.first = grid(), 
                              smooth.scatter = FALSE, lowess = FALSE, ...) {
  edgeR::maPlot(x=NULL, y=NULL,
                logAbundance=tbl$logCPM,
                logFC=tbl$logFC,
                xlab = xlab, ylab = ylab, 
                pch = pch, cex = cex, smearWidth = smearWidth,
                panel.first = panel.first, smooth.scatter = smooth.scatter, 
                lowess = lowess, 
                main=main,
              ...)
}
setMethod("smearPlot", "EdgeResult",
          function(object, contrast=NULL, freeRelation=FALSE,
                   xlab = "Average logCPM", 
                   ylab = "logFC", pch = 19, cex = 0.2, smearWidth = 0.5, panel.first = grid(), 
                   smooth.scatter = FALSE, lowess = FALSE,
                   multipage=FALSE,
                   ...) {
              tables <- dgeTableList(object, contrast)
              logFCs <- unlist(sapply(tables, function(x) x$logFC))
              logCPMs <- unlist(sapply(tables, function(x) x$logCPM))
              if(!freeRelation) {
                logFC.range <- quantile(logFCs, c(0.0005, 0.9995), na.rm=TRUE)
                logCPM.range <- quantile(logCPMs, c(0.0005, 0.9995), na.rm=TRUE)
                xlim <- logCPM.range
                ylim <- logFC.range
              }

              if(!multipage) {
                op <- ribiosPlot::compactPar()
                on.exit(par(op))
                op2 <- par(mfrow=grDevices::n2mfrow(length(tables)))
              }
              for(i in seq(along=tables)) {
                if(freeRelation) {
                  customSmearPlot(tables[[i]], main=names(tables)[i], ...)
                }  else {
                  customSmearPlot(tables[[i]], main=names(tables)[i],
                                                  xlim=xlim, ylim=ylim,
                                                  ...)
                }
              }
              if(!multipage) {
                par(op2)
              }
          })

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  corr <- cor(x,y)
  r <- abs(corr)
  scale.factor <- ifelse(r<0.2, 0.2, r)
  col <- ifelse(corr>0, "red2", "royalblue")
  txt <- format(c(corr, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * scale.factor, col=col)
}

panel.lmSmooth <- function(x,y, col = par("col"), bg = NA, pch = par("pch"), 
                      cex = 0.8, ...) {

  corr <- cor(x,y, use="complete")
  corr.col <-ifelse(corr<0, "royalblue", "red2")

  abline(h=0, v=0, col="lightgray")
  abline(lm(y~x))
  
  panel.smooth(x,y, col=col, bg=bg, pch=pch,
               cex=cex, col.smooth=corr.col, ...)

  legend("topleft",
         sprintf("r=%1.2f", corr), bty="n",
         text.col=corr.col)

}

pairs.EdgeResult <- function(x, lower.panel=panel.lmSmooth, upper.panel=panel.cor,
                             freeRelation=TRUE,
                             pch=19, ...) {
  dgeTbls <- dgeTableList(x)
  ufeat <- unique(unlist(lapply(dgeTbls, rownames)))
  logFCs <- sapply(dgeTbls, function(x) matchColumn(ufeat, x, 0L)$logFC)
  if(freeRelation) {
    xlim <- ylim <- quantileRange(logFCs, outlier=0.01)
  } else {
    xlim <- ylim <- NULL
  }
  pairs(logFCs, lower.panel=lower.panel, upper.panel=NULL, pch=pch, xlim=xlim, ylim=ylim, ...)
}

## modLogCPM (moderated log CPM)
setMethod("modLogCPM", "DGEList", function(object, prior.count=2) {
  return(cpm(object, prior.count=prior.count, log=TRUE))
})
setMethod("modLogCPM", "EdgeObject", function(object) {
  return(modLogCPM(dgeList(object)))
})

## voom
setMethod("voom", "DGEList", function(object,...) {
  limma::voom(object, ...)
})
setMethod("voom", "matrix", function(object,...) {
  limma::voom(object, ...)
})
setMethod("voom", "ExpressionSet", function(object,...) {
  limma::voom(object, ...)
})
setMethod("voom", "EdgeObject", function(object,...) {
  limma::voom(dgeList(object),
              design=designMatrix(object),
              ...)
})

## common disp
setMethod("commonDisp", "DGEList", function(object) {
  return(object$common.dispersion)
})
setMethod("commonDisp", "EdgeObject", function(object) {
  return(commonDisp(dgeList(object)))
})
setMethod("hasCommonDisp", "DGEList", function(object) {
  disp <- commonDisp(object)
  return(!is.na(disp) & !is.null(disp))
})

setMethod("hasCommonDisp", "EdgeObject", function(object) {
  return(hasCommonDisp(dgeList(object)))
})

setReplaceMethod("commonDisp", c("DGEList", "numeric"), function(object, value) {
  object$common.dispersion <- value
  return(object)
})
setReplaceMethod("commonDisp", c("EdgeObject", "numeric"), function(object, value) {
  object@dgeList$common.dispersion <- value
  return(object)
})
localSetCommonDispIfMissing <- function(object, common.disp) {
  if(!hasCommonDisp(object)) {
    commonDisp(object) <- common.disp
  }
  return(object)
}
setMethod("setCommonDispIfMissing", c("DGEList","numeric"), function(object, common.disp) {
  localSetCommonDispIfMissing(object, common.disp)
})
setMethod("setCommonDispIfMissing", c("EdgeObject","numeric"), function(object, common.disp) {
  localSetCommonDispIfMissing(object, common.disp)
})

## cpm
setMethod("cpm", "EdgeObject", function(x,...) {
              cpm(dgeList(x),...)
          })

## sniff features
setMethod("featureNames", "EdgeObject", function(object) {
  return(rownames(dgeList(object)$counts))
})

setMethod("sampleNames", "EdgeObject", function(object) {
  return(colnames(dgeList(object)$counts))
})

isValidID <- function(featNames) {
  invalid <- is.na(featNames) || featNames=="" || featNames=="-"
  return(!invalid)
}
validIDs <- function(featNames) {
  return(featNames[isValidID(featNames)])
}
likeGeneID <- function(featNames) grepl("^[0-9]*$", featNames)
likeGeneSymbol <- function(featNames) grepl("^[A-Za-z][A-Za-z0-9]*$", featNames)
likeRefSeq <- function(featNames) grepl("^[N|X][M|R|G|P]_[0-9]+\\.?[0-9]*$", featNames)
likeEnsembl <- function(featNames) grepl("^ENS[T|G|P][0-9]+$", featNames)

setMethod("sniffFeatures", "EdgeObject", function(object) {
  featNames <- featureNames(object)
  positive.thr <- 0.5
  vnames <- validIDs(featNames)
  geneIdsLike <- likeGeneID(vnames)
  if(mean(geneIdsLike)>=positive.thr) {
    return("GeneID")
  }
  geneSymbolsLike <- likeGeneSymbol(vnames)
  if(mean(geneSymbolsLike)>=positive.thr) {
    return("GeneSymbol")
  }
  refseqLike <- likeRefSeq(vnames)
  if(mean(refseqLike)>=positive.thr) {
    return("RefSeq")
  }
  ensemblLike <- likeEnsembl(vnames)
  if(mean(ensemblLike)>=positive.thr) {
    return("EnsEMBL")
  }
  return("Unknown")
})

subsetFeatures <- function(x, n=100) {
    if(length(x)<=n) return(x)
    return(sample(x, n))
}
           
likeHumanGeneSymbol <- function(x) mean(grepl("^[A-Z][A-Z0-9@orf]*$",
                                              subsetFeatures(x)), na.rm=TRUE)>=0.8

setMethod("annotate", c("EdgeObject","character", "logical"),
          function(object, target, check.target) {
            target <- match.arg(target,
                                choices=c("GeneID", "GeneSymbol", "RefSeq", "EnsEMBL", "Automatic", "Unknown"))
            if(target=="Automatic") {
              target <- sniffFeatures(object)
            }
            if(target=="Unknown") {
              ## no annotations available
              object@dgeList$genes <- NULL
              object@dgeList$annotation <- NA
              return(object)
            }
            feats <- featureNames(object)
            ## only first 100 characters are used
            feats <- substr(feats, 1, 100)
            isValidFeat <- isValidID(feats)
            if(target=="GeneID") {
              anno <- annotateGeneIDs(feats,orthologue = TRUE)
            } else if (target=="GeneSymbol") {
                ## this is very slow because of the database table look up, but is working...
              organism <- ifelse(likeHumanGeneSymbol(feats), "human", "any")
              anno <- annotateGeneSymbols(feats,organism=organism, orthologue = TRUE)
            } else if (target=="RefSeq") {
              anno <- annotateRefSeqs(feats,orthologue = TRUE)
            } else if (target=="EnsEMBL") {
              anno <- annotateEnsembl(feats,orthologue = TRUE)
            }
            if(check.target) {
              positive.thr <- 0.5
              validFeatGeneID <- anno$GeneID[isValidFeat]
              isBadGuess <- mean(is.na(validFeatGeneID))>=positive.thr
              if(isBadGuess) {
                object@dgeList$annotation <- NULL
                return(object)
              }
            }

            object@dgeList$annotation <- target
            object@dgeList$genes <- anno
            return(object)
          })
setMethod("annotate", c("EdgeObject","character", "missing"),
          function(object, target) {
            annotate(object, "Automatic", TRUE)
          })

setMethod("annotate", c("EdgeObject","missing", "missing"),
          function(object) {
            annotate(object, "Automatic")
          })

setMethod("annotation", "EdgeObject", function(object) {
  return(object@dgeList$annotation)
})

setMethod("fData", "DGEList", function(object) object$genes)
setMethod("fData<-", c("DGEList", "data.frame"), function(object, value) {
  object@genes <- value
  return(object)
})
setMethod("fData", "EdgeObject", function(object) {
  return(object@dgeList$genes)
})
setMethod("fData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$genes <- value
  return(object)
})

setMethod("pData", "DGEList", function(object) object$samples)
setMethod("pData<-", c("DGEList", "data.frame"), function(object, value) {
  object@samples <- value
  return(object)
})
setMethod("pData", "EdgeObject", function(object) {
  return(object@dgeList$samples)
})
setMethod("pData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$samples <- value
  return(object)
})

setMethod("isAnnotated", "EdgeObject", function(object) {
  return(!is.null(annotation(object)))
})
