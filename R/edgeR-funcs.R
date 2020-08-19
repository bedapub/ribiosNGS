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

## some useful attributes

#' Get settings in the significance filter
#' @param sigFilter An SigFilter object
#' @returns Numeric values of the thresholds
#' @export
posLogFC <- function(sigFilter)
  sigFilter@posLogFC

#' @rdname posLogFC
#' @export
negLogFC <- function(sigFilter)
  sigFilter@negLogFC

#' @rdname posLogFC
#' @export
aveExpr <- function(sigFilter)
  sigFilter@aveExpr

#' @rdname posLogFC
#' @export
pValue <- function(sigFilter)
  sigFilter@pValue

#' @rdname posLogFC
#' @export
FDR <- function(sigFilter)
  sigFilter@FDR

#' Tells whether the threshold was not set
#' @param sigFilter An SigFilter object
#' @returns Logical, whether the thresholds are the default values
#' @export
isUnsetPosLogFC <-
  function(sigFilter)
    posLogFC(sigFilter) == ESF_POSLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetNegLogFC <-
  function(sigFilter)
    negLogFC(sigFilter) == ESF_NEGLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetAveExpr <-
  function(sigFilter)
    aveExpr(sigFilter) == ESF_AVEEXPR_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetPValue <-
  function(sigFilter)
    pValue(sigFilter) == ESF_PVALUE_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetFDR <-
  function(sigFilter)
    FDR(sigFilter) == ESF_FDR_DEFAULT

#' Whether the SigFilter is the default one
#' @param object An SigFilter object
#' @return Logical, whether it is unset
#' @export
isUnsetSigFilter <- function(object) {
  res <- isUnsetPosLogFC(object) &
    isUnsetNegLogFC(object) &
    isUnsetAveExpr(object) &
    isUnsetPValue(object)  &
    isUnsetFDR(object)
  return(res)
}

#' Return the dgeGML method
#' @param edgeResult An \code{EdgeResult} object.
#' @export
dgeGML <- function(edgeResult)
  return(edgeResult@dgeGLM)

#' Return the SigFilter in use
#' @param edgeResult An \code{EdgeResult} object
#' @return An \code{SigFilter} object
sigFilter <- function(edgeResult)
  return(edgeResult@sigFilter)

#' Update the SigFilter
#' @param edgeResult An \code{EdgeResult} object
#' @param logFC Numeric
#' @param posLogFC Numeric
#' @param negLogFC Numeric
#' @param aveExpr Numeric
#' @param pValue Numeric
#' @param FDR Numeric
#' @return An updated \code{EdgeResult} object with updated \code{SigFilter}
#' @export
updateSigFilter <-
  function(edgeResult,
           logFC,
           posLogFC,
           negLogFC,
           aveExpr,
           pValue,
           FDR) {
    sf <- sigFilter(edgeResult)
    sf <- update(
      sf,
      logFC = logFC,
      posLogFC = posLogFC,
      negLogFC = negLogFC,
      aveExpr = aveExpr,
      pValue = pValue,
      FDR = FDR
    )
    sigFilter(edgeResult) <- sf
    return(edgeResult)
  }

#' Replace the SigFilter of an EdgeResult
#' @param edgeResult An EdgeResult object
#' @param value An SigFilter object
#' @return An updated \code{EdgeResult} object
#' @export
`sigFilter<-` <- function(edgeResult, value) {
  edgeResult@sigFilter <- value
  return(edgeResult)
}

#' Return gene count
#' @param edgeResult An EdgeResult object
#' @return Integer
#' @importFrom edgeR getCounts
#' @export
geneCount <- function(edgeResult) {
  nrow(edgeR::getCounts(dgeList(edgeResult)))
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
  if(class(sigFilter)=="EdgeSigFilter") {
    thr <- sigFilter@logCPM
  } else if(class(sigFilter)=="SigFilter") {
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
#' @param edgeResult An EdgeResult object
#' @param contrast Character, contrast(s) of interest
#' @param value Character, type of identifier returned
#' @return A vector of identifiers
#' @note TODO fix: add InputFeature
#'
#' @export
sigGene <- function(edgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSig(tbl, sf)
  tbl[issig, value]
}

#' @describeIn sigGene Only return positively significantly regulated genes
#' @export
sigPosGene <- function(edgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigPos(tbl, sf)
  tbl[issig, value]
}

#' @describeIn sigGene Only return negatively significantly regulated genes
#' @export
sigNegGene <- function(edgeResult, contrast, value = "GeneID") {
  tbl <- dgeTable(edgeResult, contrast)
  sf <- sigFilter(edgeResult)
  issig <- isSigNeg(tbl, sf)
  tbl[issig, value]
}

#' Return significantly regulated genes of all contrastsin lists
#'
#' @param edgeResult An EdgeResult object
#' @param value Character, type of identifier returned
#' @return A list of vectors of identifiers
#' @note TODO fix: add InputFeature
#'
#' @export
sigGenes <- function(edgeResult, value = "GeneID") {
  cs <- contrastNames(edgeResult)
  res <- lapply(cs, function(x)
    sigGene(edgeResult, x, value = value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return negatively significantly regulated genes
#' @export
sigPosGenes <- function(edgeResult, value = "GeneID") {
  cs <- contrastNames(edgeResult)
  res <-
    lapply(cs, function(x)
      sigPosGene(edgeResult, x, value = value))
  names(res) <- cs
  return(res)
}

#' @describeIn sigGenes Only return negatively significantly regulated genes
#' @export
sigNegGenes <- function(edgeResult, value = "GeneID") {
  cs <- contrastNames(edgeResult)
  res <-
    lapply(cs, function(x)
      sigNegGene(edgeResult, x, value = value))
  names(res) <- cs
  return(res)
}


#' Return counts of significantly regulated genes
#' @param edgeResult An EdgeResult object
#' @return A data.frame containing counts of positively and negatively regulated
#'    genes, the sum, as well as total number of features
#' @importFrom ribiosUtils ulen
#' @export
sigGeneCounts <- function(edgeResult) {
  allCount <- geneCount(edgeResult)
  posCounts <- sapply(sigPosGenes(edgeResult), ribiosUtils::ulen)
  negCounts <- sapply(sigNegGenes(edgeResult), ribiosUtils::ulen)
  total <- posCounts + negCounts
  res <- data.frame(
    posCount = posCounts,
    negCount = negCounts,
    posnegCount = posCounts + negCounts,
    all = allCount
  )
  return(res)
}

