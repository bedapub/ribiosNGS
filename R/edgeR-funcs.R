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

#' @rdname SigFilter \code{sigFilter} can be used to retrieve SigFilter objects from other objects
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

#' Return significantly regulated genes
#'
#' @param countDgeResult An EdgeResult object
#' @param contrast Character, contrast(s) of interest
#' @param value Character, type of identifier returned
#' @return A vector of identifiers
#' @note TODO fix: add InputFeature
#'
#' @export
sigGene <- function(countDgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(countDgeResult, contrast)
  sf <- sigFilter(countDgeResult)
  issig <- isSig(tbl, sf)
  tbl[issig, value]
}

#' @describeIn sigGene Only return positively significantly regulated genes
#' @export
sigPosGene <- function(countDgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(countDgeResult, contrast)
  sf <- sigFilter(countDgeResult)
  issig <- isSigPos(tbl, sf)
  tbl[issig, value]
}

#' @describeIn sigGene Only return negatively significantly regulated genes
#' @export
sigNegGene <- function(countDgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(countDgeResult, contrast)
  sf <- sigFilter(countDgeResult)
  issig <- isSigNeg(tbl, sf)
  tbl[issig, value]
}

#' Return significantly regulated genes of all contrastsin lists
#'
#' @param countDgeResult An EdgeResult object
#' @param value Character, type of identifier returned
#' @return A list of vectors of identifiers
#' @note TODO fix: add InputFeature
#'
#' @export
sigGenes <- function(countDgeResult, value = "GeneID") {
  cs <- contrastNames(countDgeResult)
  res <- lapply(cs, function(x)
    sigGene(countDgeResult, x, value = value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return negatively significantly regulated genes
#' @export
sigPosGenes <- function(countDgeResult, value = "GeneID") {
  cs <- contrastNames(countDgeResult)
  res <-
    lapply(cs, function(x)
      sigPosGene(countDgeResult, x, value = value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return negatively significantly regulated genes
#' @export
sigNegGenes <- function(countDgeResult, value = "GeneID") {
  cs <- contrastNames(countDgeResult)
  res <-
    lapply(cs, function(x)
      sigNegGene(countDgeResult, x, value = value))
  names(res) <- cs
  return(res)
}


#' Return counts of significantly regulated genes
#' @param countDgeResult An EdgeResult object
#' @return A data.frame containing counts of positively and negatively regulated
#'    genes, the sum, as well as total number of features
#' @importFrom ribiosUtils ulen
#' @export
sigGeneCounts <- function(countDgeResult) {
  allCount <- geneCount(countDgeResult)
  posCounts <- sapply(sigPosGenes(countDgeResult), ribiosUtils::ulen)
  negCounts <- sapply(sigNegGenes(countDgeResult), ribiosUtils::ulen)
  total <- posCounts + negCounts
  res <- data.frame(
    posCount = posCounts,
    negCount = negCounts,
    posnegCount = posCounts + negCounts,
    all = allCount
  )
  return(res)
}

