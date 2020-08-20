#' @include AllClasses.R AllGenerics.R

##----------------------------------------##
## plotBCV
##----------------------------------------##
#' @importFrom edgeR plotBCV
#' @describeIn plotBCV Method for DGEList
#' @export
setMethod("plotBCV", "DGEList", function(x, ...) {
  edgeR::plotBCV(x, ...)
})


#' @describeIn plotBCV Method for EdgeObject
#' @export
setMethod("plotBCV", "EdgeObject", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})


#' @describeIn plotBCV Method for EdgeResult
#' @export
setMethod("plotBCV", "EdgeResult", function(x, ...) {
  edgeR::plotBCV(dgeList(x), ...,)
})

##----------------------------------------## 
## boxplot
##----------------------------------------## 
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

#' Boxplot of an EdgeObject
#' @param x An EdgeObject
#' @param type The type of boxplot: 'normFactors' and 'modLogCPM' are supported.
#' @param xlab Character, xlab.
#' @param ylab Character, ylab.
#' @param main Character, title.
#' @param ... Passed to \code{boxplot}.
#' @importFrom graphics boxplot
#' @export
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

#' Plot distribution of normalized counts
#' @param before.norm An \code{EdgeObject} before normalization.
#' @param after.norm An \code{EdgeObject} after normalization.
#' @param ... Other parameters passed to \code{boxplot}.
#' @export
normBoxplot <- function(before.norm, after.norm, ...) {
  op <- par(mfrow=c(1,2))
  on.exit(par(op))
  boxplot(before.norm, type="modLogCPM", main="Before norm.", ...)
  boxplot(after.norm, type="modLogCPM", main="After norm.", ...)
}

#' Return a range determined by the quantile of the data
#' @param x A numeric vector
#' @param outlier Quantile (lower and higher) threshold
#' @param symmetric Logical, whether the range must be symmetric around zero.
#' @return A numeric vector of two (\code{c(low, high)}).
#' @export
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

##----------------------------------------## 
## vocalno plot
##----------------------------------------## 

#' @describeIn volcanoPlot Method for EdgeResult
#' @param contrast Character, contrast of interest. If \code{NULL}, 
#'    all contrasts are used
#' @param freeRelation Logical.
#' @param colramp Function, color palette.
#' @param multipage Logical.
#' @param yValue Character string, either \code{PValue} or \code{FDR}.
#' @param xlim NULL or a numeric vector of two
#' @param ylim NULL or a numeric vector of two.
#' @param topLabel NULL or an integer number, number of top features to be labelled
#' @param labelType NULL or a character string, a column name in the feature annotation
#' @param main Character, title.
#' @importFrom graphics smoothScatter text
#' @export
setMethod("volcanoPlot", "EdgeResult",
          function(object, contrast=NULL,
                   freeRelation=FALSE,
                   colramp=ribiosPlot::heat,
                   multipage=FALSE,
                   yValue=c("PValue", "FDR"),
                   xlim=NULL,
                   ylim=NULL,
                   main=NULL,
                   topLabel=NULL,
                   labelType=NULL,
                   ...) {
            yValue <- match.arg(yValue)
  tables <- dgeTableList(object, contrast)
  logFCs <- unlist(sapply(tables, function(x) x$logFC))
  ps <- unlist(sapply(tables, function(x) x$PValue))
  if(!freeRelation) {
    logFC.range <- quantileRange(logFCs, outlier=0.01, symmetric=TRUE)
    if(!all(ps==0)) {
      pValue.range <- quantileRange(ps[ps!=0], outlier=0.01, symmetric=FALSE)
      ylim2 <- max(-log10(pValue.range))
    } else {
      ylim2 <- 5L
    }
    if(is.null(xlim))
      xlim <- logFC.range
    if(is.null(ylim))
      ylim <- c(0, ylim2)
  }

  if(!multipage) {
    op <- ribiosPlot::compactPar()
    on.exit(par(op))
    op2 <- par(mfrow=grDevices::n2mfrow(length(tables)))
  }
  
  if(is.null(main)) {
    mains <- names(tables)
  } else {
    mains <- sprintf("%s [%s]", main, names(tables))
  }
  for(i in seq(along=tables)) {
    currTbl <- tables[[i]]
    if(yValue=="FDR") {
      yVal <- -log10(tables[[i]]$FDR)
    } else if (yValue=="PValue") {
      yVal <- -log10(tables[[i]]$PValue)
    } else {
      stop("Cannot be here")
    }
    yLab <- paste0("-log10(", yValue, ")")
    if(freeRelation) {
      with(currTbl, smoothScatter(yVal~logFC,
                                      colramp=colramp,
                                      main=mains[i],
                                      ylab=yLab,
                                      ...))
    }  else {
      with(currTbl, smoothScatter(yVal~logFC,
                                      colramp=colramp,
                                      main=mains[i],
                                      xlim=xlim, ylim=ylim,
                                      ylab=yLab,
                                      ...))
    }
    if(!is.null(topLabel)) {
      stopifnot(is.numeric(topLabel) & !is.null(labelType))
      topLabel <- as.integer(topLabel)
      topCurrTbl <- currTbl[1:topLabel,] 
      topCurrY <- yVal[1:topLabel]
      topCurrLabel <- topCurrTbl[, labelType]
      text(topCurrTbl$logFC,
           topCurrY,
           label=topCurrLabel, pos=3)
    }
    abline(h=0, col="lightgray")
    abline(v=0, col="lightgray")
  }

  if(!multipage) {
    par(op2)
  }
})


##----------------------------------------##
## smear/MA plot
##----------------------------------------##

#' Custom smear plot
#' @param tbl A \code{data.frame}
#' @param main Character string, title of the plot
#' @param xlab Character string, xlab
#' @param ylab Character string, ylab
#' @param pch Point symbol
#' @param cex Font size
#' @param smearWidth Smear with
#' @param panel.first Passed to \code{\link[edgeR]{maPlot}}.
#' @param smooth.scatter Passed to \code{\link[edgeR]{maPlot}}.
#' @param lowess Passed to \code{\link[edgeR]{maPlot}}.
#' @param ... Passed to \code{\link[edgeR]{maPlot}}.
#' @importFrom edgeR maPlot
#' @export
customSmearPlot <- function(tbl, main, 
                              xlab, 
                              ylab, pch = 19, cex = 0.2, smearWidth = 0.5, panel.first = grid(), 
                              smooth.scatter = FALSE, lowess = FALSE, ...) {
  if(missing(ylab))
    ylab <- "logFC"
  if("AveExpr" %in% colnames(tbl)) {
    abundance <- tbl$AveExpr
    if(missing(xlab)) xlab <- "Average expression"
  } else if ("logCPM" %in% colnames(tbl)) {
    abundance <- tbl$logCPM
    if(missing(xlab)) xlab <- "Average logCPM"
  } else {
    stop("Neither 'AveExpr' or 'logCPM' is found")
  }
  edgeR::maPlot(x=NULL, y=NULL,
                logAbundance=abundance,
                logFC=tbl$logFC,
                xlab = xlab, ylab = ylab, 
                pch = pch, cex = cex, smearWidth = smearWidth,
                panel.first = panel.first, smooth.scatter = smooth.scatter, 
                lowess = lowess, 
                main=main,
              ...)
}

#' @describeIn smearPlot Method for EdgeResult
#' @param contrast Character, contrast of interest
#' @param freeRelation Logical
#' @param xlab Character
#' @param ylab Character
#' @param pch Character or integer
#' @param cex Numeric
#' @param smearWidth Numeric
#' @param panel.first Grid
#' @param smooth.scatter Logical
#' @param lowess Logical
#' @param multipage Logical
#' @export
setMethod("smearPlot", "EdgeResult",
          function(object, contrast=NULL, freeRelation=FALSE,
                   xlab = "Average logCPM", 
                   ylab = "logFC", pch = 19, cex = 0.2, smearWidth = 0.5, 
                   panel.first = grid(), 
                   smooth.scatter = FALSE, lowess = FALSE,
                   multipage=FALSE,
                   ...) {
              tables <- dgeTableList(object, contrast)
              logFCs <- unlist(sapply(tables, function(x) x$logFC))
              logCPMs <- unlist(sapply(tables, function(x) x$AveExpr))
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


##----------------------------------------##
## pairs plot
##----------------------------------------##

#' Pairs plot for EdgeResult
#' @param x An EdgeResult object.
#' @param lower.panel Lower panel, passed to \code{pairs}.
#' @param upper.panel Upper panel, passed to \code{pairs}.
#' @param freeRelation Logical, whether x- and y-axis shoule have the same range
#' @param pch Point symbol
#' @param ... passed to \code{pairs}
#'
#' Plot pairwise logFCs
#'
#' @seealso \code{\link[graphics]{pairs}}. 
#' @importFrom graphics pairs
#' @importFrom ribiosPlot panel.cor panel.lmSmooth
#' @export
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

##------------------------------##
## plotMDS
##------------------------------##

#' plotMDS for EdgeObject
#' @param x An EdgeObject object
#' @param ... Other parameters passed to \code{\link[limma]{plotMDS}}.
#' @importFrom limma plotMDS
#' @export
plotMDS.EdgeObject <- function(x,  ...) {
  plotMDS(dgeList(x), ...)
}
