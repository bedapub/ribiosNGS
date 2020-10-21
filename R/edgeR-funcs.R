#' @include AllClasses.R AllGenerics.R AllMethods.R
NULL

#' Sample counts by group
#' @param edgeObj An EdgeObject object
#' @aliases maxCountByGroup hasNoReplicate
#' @return A named vector if integers, sample counts by group
#' @export
countByGroup <- function(edgeObj) {
  groups <- groups(edgeObj@designContrast)
  if (!is.factor(groups))
    groups <- factor(groups)
  groups <- droplevels(groups)
  return(table(groups))
}

#' @describeIn countByGroup Returns the max count
#' @export
maxCountByGroup <- function(edgeObj) {
  return(max(countByGroup(edgeObj)))
}

#' @describeIn countByGroup Returns \code{TRUE} if the largest group has only one sample
#' @export
hasNoReplicate <- function(edgeObj) {
  return(maxCountByGroup(edgeObj) <= 1)
}

isAnyNA <- function(edgeObj) {
  any(is.na(edgeObj@dgeList$counts))
}

#' Replace NA counts with zero counts
#' @param edgeObj An EdgeObject object
#' @return An EdgeObject object
#' @export
replaceNAwithZero <- function(edgeObj) {
  edgeObj@dgeList$counts[is.na(edgeObj@dgeList$counts)] <- 0
  return(edgeObj)
}

#' Return the dgeGML method
#' @param edgeResult An \code{EdgeResult} object.
#' @export
dgeGML <- function(edgeResult)
  return(edgeResult@dgeGLM)

#' @rdname SigFilter can be used to retrieve SigFilter objects from other objects
#' Return the SigFilter in use
#' @param countDgeResult An \code{countDgeResult} object
#' @return An \code{SigFilter} object
sigFilter <- function(countDgeResult)
  return(countDgeResult@sigFilter)

#' Update the SigFilter
#' @param countDgeResult An \code{EdgeResult} or \code{LimmaVoomResult} object
#' @param logFC Numeric
#' @param posLogFC Numeric
#' @param negLogFC Numeric
#' @param pValue Numeric
#' @param FDR Numeric
#' @param ... Other parameters, now used ones including \code{aveExpr} (for LimmaSigFilter) and \code{logCPM} (for EdgeSigFilter)
#' @return An updated \code{CountDgeResult} object with updated \code{SigFilter}
#' @export
updateSigFilter <-
  function(countDgeResult,
           logFC,
           posLogFC,
           negLogFC,
           pValue,
           FDR, ...) {
    sf <- sigFilter(countDgeResult)
    sf <- update(
      sf,
      logFC = logFC,
      posLogFC = posLogFC,
      negLogFC = negLogFC,
      pValue = pValue,
      FDR = FDR,
      ...
    )
    sigFilter(countDgeResult) <- sf
    return(countDgeResult)
  }


#' Replace the SigFilter of an CountDgeResult
#' @param countDgeResult An \code{EdgeResult} or \code{LimmaVoomResult} object
#' @param value An SigFilter object
#' @return An updated \code{countDgeResult} object
#' @export
`sigFilter<-` <- function(countDgeResult, value) {
  countDgeResult@sigFilter <- value
  return(countDgeResult)
}

#' Return gene count
#' @param countDgeResult An EdgeResult object
#' @return Integer
#' @importFrom edgeR getCounts
#' @export
geneCount <- function(countDgeResult) {
  nrow(edgeR::getCounts(dgeList(countDgeResult)))
}

#' Assert that the input data.frame is a valid EdgeTopTable
#' @param x A data.frame
#' @return Logical
#' @export
assertEdgeToptable <- function(x) {
  stopifnot(is.data.frame(x)
            &
              all(c("logFC", "PValue", "FDR") %in% colnames(x)) &
              any(c("AveExpr", "logCPM") %in% colnames(x)))
}

isHighAveExpr <- function(data.frame, sigFilter) {
  if(class(sigFilter)=="SigFilter") { ## SigFilter does not support filtering by AveExpr
    isAveExpr <- rep(TRUE, nrow(data.frame))
  } else {
    if(class(sigFilter)=="EdgeSigFilter") {
      thr <- sigFilter@logCPM
    } else if(class(sigFilter)=="LimmaSigFilter") {
      thr <- sigFilter@aveExpr
    } else {
      stop("Slot 'aveExpr' or 'logCPM' was not found in sigFilter.")
    }
    if("AveExpr" %in% colnames(data.frame)) {
      isAveExpr <- with(data.frame, AveExpr >= thr)
    } else if ("logCPM" %in% colnames(data.frame)) {
      isAveExpr <- with(data.frame, logCPM >= thr)
    } else {
      stop("Column 'AveExpr' or 'logCPM' was not found.")
    } 
  }
  return(isAveExpr)
}

#' Return logical vector indicating which genes are significantly regulated
#' @param data.frame A \code{data.frame} that must pass \code{assertEdgeToptable}
#' @param sigFilter An SigFilter object
#' @returns A logical vector of the same length as the row number of the input data.frame
#' @export
isSig <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  resExcAveExpr <- with(data.frame,
    (logFC >= posLogFC(sigFilter) |
       logFC <= negLogFC(sigFilter)) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
  
  isAveExpr <- isHighAveExpr(data.frame, sigFilter)
  res <- resExcAveExpr & isAveExpr
  return(res)
}

#' @describeIn isSig Returns which genes are significantly positively regulated
#' @export
isSigPos <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  resExcAveExpr <- with(
    data.frame,
    logFC >= posLogFC(sigFilter) &
      AveExpr >= aveExpr(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
  isAveExpr <- isHighAveExpr(data.frame, sigFilter)
  res <- resExcAveExpr & isAveExpr
  return(res)
}

#' @describeIn isSig Returns which genes are significantly negatively regulated
#' @export
isSigPos <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  resExcAveExpr <- with(
    data.frame,
    logFC >= posLogFC(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
  isAveExpr <- isHighAveExpr(data.frame, sigFilter)
  res <- resExcAveExpr & isAveExpr
  return(res)
}

isSigNeg <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  resExcAveExpr <- with(
    data.frame,
    logFC <= negLogFC(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
  isAveExpr <- isHighAveExpr(data.frame, sigFilter)
  res <- resExcAveExpr & isAveExpr
  return(res)
}

#' Return gene identifier types
#' @param dgeResult An DgeResult object
#' @return A character string indicating the gene identifiers found
#' The following terms are recognized: GeneID, EnsemblID, GeneSymbol, FeatureName
#' @export
geneIdentifierTypes <- function(dgeResult) {
  fcnames <- colnames(fData(dgeResult))
  keywords <- c("GeneID", "EnsemblID", "GeneSymbol", "FeatureName")
  res <- intersect(keywords, fcnames)
  return(res)
}

#' Return gene identifiers of significant DGEs
#' @param dgeResult An DgeResult object.
#' @param contrast A character string, a contrast of interest.
#' @param sigFunc A function, defining the type of significant genes.
#' @param value \code{NULL} or character string, if not \code{NULL}, it must be a column name in the feature annotation data.
#' 
#' @return A vector of character strings indicating the gene identifiers that are significantly regulated. If no defined types are found, either rownames or the first column is returned
#' @seealso \code{\link{geneIdentifierTypes}}
#' @export
sigGeneIdentifiers <- function(dgeResult, contrast, sigFunc=isSig, value=NULL) {
  tbl <- dgeTable(dgeResult, contrast)
  sf <- sigFilter(dgeResult)
  issig <- do.call(sigFunc, list(tbl, sf))

  if(!is.null(value)) {
    haltifnot(value %in% colnames(fData(dgeResult)),
              msg=sprintf("%s not found in feature annotation", value))
    idtypes <- value
  } else {
    idtypes <- geneIdentifierTypes(dgeResult)
  }
  if(length(idtypes)>0) {
    res <- tbl[issig, idtypes[1]]
  } else {
    if(!is.null(rownames(tbl))) {
      res <- rownames(tbl)[issig]
    } else {
      res <- tbl[issig, 1L]
    }
  }
  return(res)
}

#' Return significantly regulated genes
#'
#' @param countDgeResult An EdgeResult object
#' @param contrast Character, contrast(s) of interest
#' @param value \code{NULL} or character string, if not \code{NULL}, it must be a column name in the feature annotation data.
#' @return A vector of identifiers
#'
#' @examples 
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exMat[2:4, 4:6] <- exMat[2:4, 4:6]+20
#' exMat[7:9, 1:3] <- exMat[7:9, 1:3]+20
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' colnames(exDesign) <- levels(exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(GeneID=1:nrow(exMat),
#'   GeneSymbol=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                    fData=exFdata, pData=exPdata)
#' exDgeRes <- dgeWithEdgeR(exObj)
#' sigGenes(exDgeRes)
#' sigPosGenes(exDgeRes)
#' sigNegGenes(exDgeRes)
#' ## specify the value type to return
#' sigGenes(exDgeRes, value="GeneSymbol")
#' sigPosGenes(exDgeRes, value="GeneSymbol")
#' sigNegGenes(exDgeRes, value="GeneSymbol")
#' @export
sigGene <- function(countDgeResult, contrast, value=NULL) {
  res <- sigGeneIdentifiers(countDgeResult, contrast, sigFunc=isSig, value=value)
  return(res)
}

#' @describeIn sigGene Only return positively significantly regulated genes
#' @export
sigPosGene <- function(countDgeResult, contrast, value=NULL) {
  res <- sigGeneIdentifiers(countDgeResult, contrast, sigFunc=isSigPos, value=value)
  return(res)
}

#' @describeIn sigGene Only return negatively significantly regulated genes
#' @export
sigNegGene <- function(countDgeResult, contrast, value=NULL) {
  res <- sigGeneIdentifiers(countDgeResult, contrast, sigFunc=isSigNeg, value=value)
  return(res)
}

#' Return significantly regulated genes of all contrasts
#'
#' @param countDgeResult An EdgeResult object
#' @param value \code{NULL} or character string, if not \code{NULL}, it must be a column name in the feature annotation data.
#' @return A list of vectors of identifiers
#' @note TODO fix: add InputFeature
#'
#' @export
sigGenes <- function(countDgeResult, value=NULL) {
  cs <- contrastNames(countDgeResult)
  res <- lapply(cs, function(x)
    sigGene(countDgeResult, x, value=value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return significantly positively regulated genes
#' @export
sigPosGenes <- function(countDgeResult, value=NULL) {
  cs <- contrastNames(countDgeResult)
  res <-
    lapply(cs, function(x)
      sigPosGene(countDgeResult, x, value=value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return significantly negatively regulated genes
#' @export
sigNegGenes <- function(countDgeResult, value=NULL) {
  cs <- contrastNames(countDgeResult)
  res <-
    lapply(cs, function(x)
      sigNegGene(countDgeResult, x, value=value))
  names(res) <- cs
  return(res)
}


#' Return counts of significantly regulated genes
#' @param countDgeResult An EdgeResult object
#' @param value \code{NULL} or character string, if not \code{NULL}, it must be a column name in the feature annotation data.
#' @return A data.frame containing counts of positively and negatively regulated
#'    genes, the sum, as well as total number of features
#' @importFrom ribiosUtils ulen
#' @examples 
#' exMat <- matrix(rpois(120, 10), nrow=20, ncol=6)
#' exMat[2:4, 4:6] <- exMat[2:4, 4:6]+20
#' exMat[7:9, 1:3] <- exMat[7:9, 1:3]+20
#' exGroups <- gl(2,3, labels=c("Group1", "Group2"))
#' exDesign <- model.matrix(~0+exGroups)
#' colnames(exDesign) <- levels(exGroups)
#' exContrast <- matrix(c(-1,1), ncol=1, dimnames=list(c("Group1", "Group2"), c("Group2.vs.Group1")))
#' exDescon <- DesignContrast(exDesign, exContrast, groups=exGroups)
#' exFdata <- data.frame(GeneSymbol=sprintf("Gene%d", 1:nrow(exMat)))
#' exPdata <- data.frame(Name=sprintf("Sample%d", 1:ncol(exMat)),
#'                      Group=exGroups)
#' exObj <- EdgeObject(exMat, exDescon, 
#'                    fData=exFdata, pData=exPdata)
#' exDgeRes <- dgeWithEdgeR(exObj)
#' sigGeneCounts(exDgeRes)
#' @export
sigGeneCounts <- function(countDgeResult, value=NULL) {
  allCount <- geneCount(countDgeResult)
  posCounts <- sapply(sigPosGenes(countDgeResult), ribiosUtils::ulen)
  negCounts <- sapply(sigNegGenes(countDgeResult), ribiosUtils::ulen)
  total <- posCounts + negCounts
  res <- data.frame(
    Contrast=contrastNames(countDgeResult),
    posCount = posCounts,
    negCount = negCounts,
    posnegCount = posCounts + negCounts,
    all = allCount,
    row.names=NULL
  )
  return(res)
}

