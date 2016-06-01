#' @importFrom methods new
#' @importFrom utils assignInNamespace getFromNamespace read.table


#' @export
setQClabel <- function(eset, labels) {
  eset$QC_LABEL <- labels
  return(eset)
}

#' @export
setQCgroup <- function(eset, group) {
  eset$QC_GROUP <- group
  return(eset)
}

#' @export
setNormGroup <- function(eset, group) {
  eset$QC_NORMGRP <- group
  return(eset)
}

#' @export
getQClabel <- function(eset) {
  labels <- eset$QC_LABEL
  if(is.null(labels))
    labels <- sampleNames(eset)
  return(labels)
}

#' @export
getQCgroup <- function(eset) {
  group <- eset$QC_GROUP
  if(is.null(group))
    group <- rep("Undefined", ncol(eset))
  if(!is.factor(group)) group <- factor(group)
  return(group)
}

#' @export
getNormGroup <- function(eset) {
  group <- eset$QC_NORMGRP
  if(is.null(group))
    group <- rep("Undefined", ncol(eset))
  if(!is.factor(group)) group <- factor(group)
  return(group)
}

#' @export
cel2samplename <- function(x) {
  x <- gsub("\\.cel$", "", x, ignore.case=TRUE)
  x <- gsub(".*\\\\", "", x)
  return(x)
}
