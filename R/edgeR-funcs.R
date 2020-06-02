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
#' @param edgeSigFilter An EdgeSigFilter object
#' @returns Numeric values of the thresholds
#' @export
posLogFC <- function(edgeSigFilter)
  edgeSigFilter@posLogFC

#' @rdname posLogFC
#' @export
negLogFC <- function(edgeSigFilter)
  edgeSigFilter@negLogFC

#' @rdname posLogFC
#' @export
logCPM <- function(edgeSigFilter)
  edgeSigFilter@logCPM

#' @rdname posLogFC
#' @export
LR <- function(edgeSigFilter)
  edgeSigFilter@LR
pValue <- function(edgeSigFilter)
  edgeSigFilter@pValue

#' @rdname posLogFC
#' @export
FDR <- function(edgeSigFilter)
  edgeSigFilter@FDR

#' Tells whether the threshold was not set
#' @param edgeSigFilter An EdgeSigFilter object
#' @returns Logical, whether the thresholds are the default values
#' @export
isUnsetPosLogFC <-
  function(edgeSigFilter)
    posLogFC(edgeSigFilter) == ESF_POSLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetNegLogFC <-
  function(edgeSigFilter)
    negLogFC(edgeSigFilter) == ESF_NEGLOGFC_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetLogCPM <-
  function(edgeSigFilter)
    logCPM(edgeSigFilter) == ESF_LOGCPM_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetLR <-
  function(edgeSigFilter)
    LR(edgeSigFilter) == ESF_LR_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetPValue <-
  function(edgeSigFilter)
    pValue(edgeSigFilter) == ESF_PVALUE_DEFAULT

#' @rdname isUnsetPosLogFC
#' @export
isUnsetFDR <-
  function(edgeSigFilter)
    FDR(edgeSigFilter) == ESF_FDR_DEFAULT

#' Whether the EdgeSigFilter is the default one
#' @param object An EdgeSigFilter object
#' @return Logical, whether it is unset
#' @export
isUnsetSigFilter <- function(object) {
  res <- isUnsetPosLogFC(object) &
    isUnsetNegLogFC(object) &
    isUnsetLogCPM(object) &
    isUnsetLR(object) &
    isUnsetPValue(object)  &
    isUnsetFDR(object)
  return(res)
}

#' Return the dgeGML method
#' @param edgeResult An \code{EdgeResult} object.
#' @export
dgeGML <- function(edgeResult)
  return(edgeResult@dgeGLM)

#' Return the EdgeSigFilter in use
#' @param edgeResult An \code{EdgeResult} object
#' @return An \code{EdgeSigFilter} object
sigFilter <- function(edgeResult)
  return(edgeResult@sigFilter)

#' Update the EdgeSigFilter
#' @param edgeResult An \code{EdgeResult} object
#' @param logFC Numeric
#' @param posLogFC Numeric
#' @param negLogFC Numeric
#' @param logCPM Numeric
#' @param LR Numeric
#' @param pValue Numeric
#' @param FDR Numeric
#' @return An updated \code{EdgeResult} object with updated \code{EdgeSigFilter}
#' @export
updateSigFilter <-
  function(edgeResult,
           logFC,
           posLogFC,
           negLogFC,
           logCPM,
           LR,
           pValue,
           FDR) {
    sf <- sigFilter(edgeResult)
    sf <- update(
      sf,
      logFC = logFC,
      posLogFC = posLogFC,
      negLogFC = negLogFC,
      logCPM = logCPM,
      LR = LR,
      pValue = pValue,
      FDR = FDR
    )
    sigFilter(edgeResult) <- sf
    return(edgeResult)
  }

#' Replace the EdgeSigFilter of an EdgeResult
#' @param edgeResult An EdgeResult object
#' @param value An EdgeSigFilter object
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
              all(c("logFC", "logCPM", "LR", "PValue", "FDR") %in% colnames(x)))
}

#' Return logical vector indicating which genes are significantly regulated
#' @param data.frame A \code{data.frame} that must pass \code{assertEdgeToptable}
#' @param sigFilter An EdgeSigFilter object
#' @returns A logical vector of the same length as the row number of the input data.frame
#' @export
isSig <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(
    data.frame,
    (logFC >= posLogFC(sigFilter) |
       logFC <= negLogFC(sigFilter)) &
      logCPM >= logCPM(sigFilter) &
      LR >= LR(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
}

#' @describeIn isSig Returns which genes are significantly positively regulated
#' @export
isSigPos <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(
    data.frame,
    logFC >= posLogFC(sigFilter) &
      logCPM >= logCPM(sigFilter) &
      LR >= LR(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
  
}

#' @describeIn isSig Returns which genes are significantly negatively regulated
#' @export
isSigPos <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(
    data.frame,
    logFC >= posLogFC(sigFilter) &
      logCPM >= logCPM(sigFilter) &
      LR >= LR(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
}

isSigNeg <- function(data.frame, sigFilter) {
  assertEdgeToptable(data.frame)
  with(
    data.frame,
    logFC <= negLogFC(sigFilter) &
      logCPM >= logCPM(sigFilter) &
      LR >= LR(sigFilter) &
      PValue <= pValue(sigFilter) &
      FDR <= FDR(sigFilter)
  )
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

