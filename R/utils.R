setQClabel <- function(eset, labels) {
  eset$QC_LABEL <- labels
  return(eset)
}
setQCgroup <- function(eset, group) {
  eset$QC_GROUP <- group
  return(eset)
}
setNormGroup <- function(eset, group) {
  eset$QC_NORMGRP <- group
  return(eset)
}
getQClabel <- function(eset) {
  labels <- eset$QC_LABEL
  if(is.null(labels))
    labels <- sampleNames(eset)
  return(labels)
}
getQCgroup <- function(eset) {
  group <- eset$QC_GROUP
  if(is.null(group))
    group <- rep("Undefined", ncol(eset))
  if(!is.factor(group)) group <- factor(group)
  return(group)
}
getNormGroup <- function(eset) {
  group <- eset$QC_NORMGRP
  if(is.null(group))
    group <- rep("Undefined", ncol(eset))
  if(!is.factor(group)) group <- factor(group)
  return(group)
}
cel2samplename <- function(x) {
  x <- gsub("\\.cel$", "", x, ignore.case=TRUE)
  x <- gsub(".*\\\\", "", x)
  return(x)
}
