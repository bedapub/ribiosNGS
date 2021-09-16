DIRCOLS <- c("Any"="black", "Positive"="#B2182B", "Negative"="#2066AC")

globalVariables(c("DIRCOLS", "logFC", "group", "Direction", "GeneIdentifier"))

#' Plot top significantly differentially expressed genes by contrast
#' @param countDgeResult A \code{CountDgeResult} object, for instance \code{EdgeResult} or \code{LimmaVoomDgeResult}.
#' @param contrast A character string, or an index (integer), or a logical vector with one \code{TRUE} element, to indicate which contrast to plot
#' @param n Integer, how many genes should be visualized.
#' @param nSigned \code{NULL} or integer, in the later case the top \code{nSigned} genes from positively and negatively regulated genes are shown, respectively.
#' @param identifier Character string, column name in \code{genes} annotation to be used to index and display the genes.
#' 
#' @return A \code{ggplot} object. If \code{nSigned} is not \code{NULL}, genes are 
#'   plotted with colors: blue indicating down-regulated genes and red indicating up-regulated genes.
#' @seealso \code{\link{plotTopSigGenesByContrast}} plots all contrasts at once.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter vars
#' @importFrom ggplot2 ggplot aes scale_color_manual facet_wrap geom_point geom_boxplot xlab ylab theme ggtitle element_text
#' 
#' @examples 
#' edgeObj <- exampleEdgeObject()
#' edgeRes <- dgeWithEdgeR(edgeObj)
#' plotTopSigGenesByContrast(edgeRes, n=6, contrast=1)
#' plotTopSigGenesByContrast(edgeRes, n=6, contrast=2)
#' ## display top three positive and top three negative genes
#' plotTopSigGenesByContrast(edgeRes, nSigned=3, contrast=1)
#' plotTopSigGenesByContrast(edgeRes, nSigned=4, contrast=2)
#' @export
plotTopSigGenesByContrast <- function(countDgeResult, 
                                      contrast, 
                                      n=5,
                                      nSigned=NULL,
                                      identifier="GeneSymbol") {
  stopifnot(!missing(contrast))
  if(is.logical(contrast) || is.numeric(contrast)) {
    contrast <- contrastNames(countDgeResult)[contrast]
  }
  dgeTbl <- dgeTable(countDgeResult, contrast)
  if(is.null(nSigned)) {
    dgeTbl <- head(dgeTbl, n)
  } else {
    dgePosTbl <- dgeTbl %>% dplyr::filter(logFC>0) %>% head(nSigned)
    dgeNegTbl <- dgeTbl %>% dplyr::filter(logFC<0) %>% head(nSigned)
    dgeTbl <- rbind(dgePosTbl, dgeNegTbl)
  }
  dgeListObj <- dgeList(countDgeResult)
  stopifnot(identifier %in% colnames(dgeTbl) &&
              identifier %in% colnames(dgeListObj$genes))
  topGenes <- dgeTbl[, identifier]
  topGeneDgeList <- dgeListObj[dgeListObj$genes[, identifier] %in% topGenes, ]
  tgLongDf <- DGEListToLongTable(topGeneDgeList)
  tgLongDf$GeneIdentifier <-  factor(tgLongDf[, identifier],
                                     unique(topGenes))
  
  if(is.null(nSigned)) {
    tgLongDf <- tgLongDf %>% dplyr::mutate(Direction="Any")
  } else {
    tgLongDf <- tgLongDf %>% dplyr::mutate(Direction="Positive") %>%
      dplyr::mutate(Direction=replace(Direction,
                               GeneIdentifier %in% dgeNegTbl[, identifier],
                               "Negative"))
  }
  
  res <- ggplot2::ggplot(tgLongDf, aes(x=group, y=exprs, color=Direction)) +
    scale_color_manual(values=DIRCOLS) +
    facet_wrap(~GeneIdentifier, nrow=2L) +
    geom_boxplot() + geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
          legend.position = ifelse(is.null(nSigned), "none", "right")) +
    xlab("Sample group") + ylab("Expression (log2cpm)") +
    ggtitle("Top differentially expressed genes",
            subtitle = paste("Contrast:", contrast))
  return(res)
}

#' Plot top significantly differentially expressed genes by contrast
#' @param countDgeResult A \code{CountDgeResult} object, for instance \code{EdgeResult} or \code{LimmaVoomDgeResult}.
#' @param n Integer, how many genes should be visualized per contrast.
#' @param nSigned \code{NULL} or integer, in the later case the top \code{nSigned} genes from positively and negatively regulated genes are shown per contrast, respectively.
#' @param identifier Character string, column name in \code{genes} annotation to be used to index and display the genes.
#' 
#' @return A \code{ggplot} object
#' @seealso \code{\link{plotTopSigGenesByContrast}} plots one contrast at a time.
#' 
#' @importFrom ggplot2 ggplot aes scale_color_manual facet_wrap geom_point geom_boxplot xlab ylab theme ggtitle
#' 
#' @examples 
#' edgeObj <- exampleEdgeObject()
#' edgeRes <- dgeWithEdgeR(edgeObj)
#' plotTopSigGenes(edgeRes, n=6)
#' ## display top two positive and top three negative genes
#' plotTopSigGenes(edgeRes, nSigned=2)
#' @export
plotTopSigGenes <- function(countDgeResult, 
                            n=5, 
                            nSigned=NULL,
                            identifier="GeneSymbol") {
  allContrasts <- contrastNames(countDgeResult)
  if(any(duplicated(allContrasts))) {
    warning("Duplicated contrasts detected and removed:",
            paste(allContrasts[duplicated(allContrasts)], collapse=","))
    allContrasts <- unique(allContrasts)
  }
  contrastPlots <- lapply(allContrasts, function(ctr) {
    plotTopSigGenesByContrast(countDgeResult, 
                              contrast=ctr,
                              n=n,
                              nSigned=nSigned, 
                              identifier=identifier)
  })
  longdfs <- lapply(contrastPlots, function(x) x$data)
  identifierLevels <- unique(unlist(lapply(longdfs, function(x) levels(x$GeneIdentifier))))
  longdf <- cbind(Contrast=factor(rep(allContrasts, sapply(longdfs, nrow)),
                                  allContrasts),
               do.call(rbind, longdfs))
  longdf$GeneIdentifier <- factor(longdf$GeneIdentifier,
                                  identifierLevels)
  res <- ggplot(longdf, aes(x=group, y=exprs, color=Direction)) +
    scale_color_manual(values=DIRCOLS) +
    facet_wrap(vars(Contrast, GeneIdentifier),
               nrow=length(allContrasts)) +
    geom_boxplot() + geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
          legend.position = ifelse(is.null(nSigned), "none", "right")) +
    xlab("Sample group") + ylab("Expression (log2cpm)") +
    ggtitle("Top differentially expressed genes")
  return(res)
}
