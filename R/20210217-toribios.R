#' @include AllClasses.R AllGenerics.R AllMethods.R
NULL

utils::globalVariables(c("FDRScore", "GeneSymbol", "pScore", "value", "variable"))

#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr slice_min group_by
newVolcanoPlot <- function(edgeRes, 
                           contrasts=NULL, 
                           addSigFilterLines=FALSE,
                           n=10) {
  if(is.null(contrasts))
    contrasts <- contrastNames(edgeRes)
  dgeTbl <- dgeTable(edgeRes, contrasts) %>%
    mutate(Contrast=factor(Contrast, contrasts),
           FDRScore=pScore(FDR, method="absLog10"))
  dgeTblHighlightData <- dgeTbl %>% group_by(Contrast) %>% dplyr::slice_min(n=n, order_by=FDR)
  ## lfcQuant <- ribiosPlot::symrange(with(dgeTbl, quantile(logFC, c(0.001,0.999))))
  ## fdrVal <- with(dgeTbl, pScore(quantile(FDR, c(0.001)), method="absLog10"))
  resScatter <- ggplot(dgeTbl, aes(x=logFC, y=FDRScore)) + 
    geom_point(pch=".", size=2) + facet_wrap(~Contrast) +
    geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    ggrepel::geom_text_repel(data=dgeTblHighlightData,
                             aes(x=logFC, y=FDRScore, 
                                 col=factor(sign(logFC)),
                                 label=GeneSymbol),
                             force=3, size=3) +
    scale_fill_manual(values=c("#D1E5F0", "#FDDAC6"), name="sign(logFC)") +
    scale_color_manual(values=c("#2066AC", "#B2182B"), name="sign(logFC)") +
    geom_point(data=dgeTblHighlightData,
               show.legend = FALSE, 
               aes(x=logFC, y=-log10(FDR), fill=factor(sign(logFC))), pch=21, size=2) +
    ggtitle("Volcano plot") +
    theme(legend.position = "none")
  if(addSigFilterLines) {
    posLogFCthr <- edgeRes@sigFilter@posLogFC
    negLogFCthr <- edgeRes@sigFilter@negLogFC
    fdrThr <- edgeRes@sigFilter@FDR
    resScatter <- resScatter + 
      geom_hline(yintercept = pScore(fdrThr, method="absLog10"), linetype=2, col="orange") +
      geom_vline(xintercept = posLogFCthr, linetype=2) +
      geom_vline(xintercept = negLogFCthr, linetype=2) 
  }
  return(resScatter)
}

clipVolcanoPlot <- function(newVolcanoPlot, ylim=c(0,30), xlim=c(-3,3)) {
  res <- newVolcanoPlot + coord_cartesian(xlim=xlim, ylim=ylim) +
    ggtitle("Volcano plot", subtitle = "Clipped")
  return(res)
}

#' @importFrom reshape2 melt
#' @importFrom ribiosUtils relevels
#' @importFrom ribiosPlot royalredblue
#' @importFrom ggplot2 geom_bar geom_vline coord_cartesian
newSigGeneBarchart <- function(edgeResult, contrasts=NULL) {
  if(is.null(contrasts))
    contrasts <- contrastNames(edgeResult)
  counts <- sigGeneCounts(edgeResult) %>%
    filter(Contrast %in% contrasts) %>%
    mutate(Contrast=factor(Contrast, contrasts)) %>%
    reshape2::melt(id.vars="Contrast") %>%
    filter(variable %in% c("posCount", "negCount")) %>%
    mutate(variable=ribiosUtils::relevels(variable, c("posCount"="Positive", "negCount"="Negative")))
  ggplot(counts, aes(x=Contrast, y=value, fill=variable, group=variable)) +
    geom_bar(stat="identity", position="dodge2") +
    scale_fill_manual(values=ribiosPlot::royalredblue(5)[c(1,5)],name="Regulation") +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) +
    ylab("Gene count") +
    ggtitle("Count of significantly regulated genes",
            subtitle=sprintf("Filtering criteria: (logFC>%g OR logFC<%g) AND FDR<%g",
                     edgeResult@sigFilter@posLogFC,
                     edgeResult@sigFilter@negLogFC,
                     edgeResult@sigFilter@FDR)) ## todo: print sigFilter as a string
}




.sigFilterLabel <- function(sigfilter,
                            type=c("sig", "pos", "neg")) {
  if(missing(type))
    type <- "sig"
  type <- match.arg(type)
  base <- sprintf("logCPM>=%g & FDR<=%g", sigfilter@logCPM, sigfilter@FDR)
  if(type=="sig") {
    if(abs(sigfilter@posLogFC)==abs(sigfilter@negLogFC)) {
      lfcLabel <- sprintf("& |logFC|>=%g", abs(sigfilter@posLogFC))
    } else {
      lfcLabel <- sprintf(" & (logFC>=%g | logFC<=%g)", sigfilter@posLogFC, sigfilter@negLogFC)
    }
  } else if (type=="pos") {
    lfcLabel <- sprintf("& logFC>=%g", sigfilter@posLogFC)
  } else if (type=="neg") {
    lfcLabel <- sprintf("& logFC<=%g", sigfilter@negLogFC)
  } else {
    stop("should not be here")
  }
  res <- paste(base, lfcLabel, sep=" ")
  return(res)
}

setGeneric("sigFilterLabel", function(object, type) standardGeneric("sigFilterLabel"))
setMethod("sigFilterLabel", "SigFilter", function(object, type) .sigFilterLabel(object, type))
setMethod("sigFilterLabel", "CountDgeResult", function(object, type) .sigFilterLabel(object@sigFilter, type))


#' @importFrom UpSetR upset fromList
sigUpsetByContrast <- function(edgeRes, contrasts=NULL, value=NULL,
                               type=c("sig", "pos", "neg"),
                               mainbar.y.label=NULL, text.scale=1.25,
                               ...) {
  type <- match.arg(type)
  if(is.null(contrasts))
    contrasts <- contrastNames(edgeRes)
  if(missing(mainbar.y.label) || is.null(mainbar.y.label)) {
    mainbar.y.label <- sprintf("DGE (%s)", sigFilterLabel(edgeRes, type=type))
  }
  if(type=="sig") {
    genes <- sigGenes(edgeRes, value=value)[contrasts]
  } else if (type=="pos") {
    genes <- sigPosGenes(edgeRes, value=value)[contrasts]
  } else if (type=="neg") {
    genes <- sigNegGenes(edgeRes, value=value)[contrasts]
  } else {
    stop("should not be here")
  }
  if(type!="sig") {
    names(genes) <- paste(names(genes), type, sep=".")
  }

  res <- UpSetR::upset(UpSetR::fromList(genes),
                       nsets=length(genes), 
                       mainbar.y.label=mainbar.y.label, 
                       sets=rev(names(genes)), ## rev because display order in matrix rows is reversed (bottom to up)
                       text.scale=text.scale,
                       ...) 
  return(res)
}


allSigUpsetByContrast <- function(edgeRes, contrasts=NULL, value=NULL, 
                                  mainbar.y.label=NULL,
                                  text.scale=1.25,
                                  ...) {
  res <- lapply(c("sig", "pos", "neg"), function(type) 
    sigUpsetByContrast(edgeRes, contrasts=contrasts, value=value,
                       type=type, mainbar.y.label=mainbar.y.label, text.scale=text.scale,
                       ...))
  return(res)
}
