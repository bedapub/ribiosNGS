getSampleAnnotations <- function(isaRes, eset, xref, modules) {
  if(length(xref)==1) {
    if(xref %in% colnames(pData(eset))) {
      xrefs <- pData(eset)[,xref]
    } else if (is.numeric(xref) && xref < ncol(pData(eset)) && xref > 0) {
      xrefs <- pData(eset)[, xref]
    } else {
      stop("When of length 1, \"xref\" must be the column name of xrefs or the column index")
    }
  } else {
    if(length(xref) != dim(eset)[2]) {
      stop("xref length not equal to the eset sample size")
    } else {
     xrefs <- xref
    }
  }
  
  if(missing(modules))
    modules <- seq_len(ncol(isaRes@genes))

  sample.names <- getSampleNames(isaRes, modules)
  sample.xrefs <- lapply(sample.names, function(x) xrefs[match(x, sampleNames(eset))])
  return(sample.xrefs)
}
getSampleClass <- function(isaRes, eset, class, modules) {
  .Deprecated("getSampleAnnotations")
}

getFeatureAnnotations <- function(isaRes, eset, xref, modules) {
  if(length(xref)==1) {
    if(xref %in% colnames(fData(eset))) {
      xrefs <- fData(eset)[,xref]
    } else if (is.numeric(xref) && xref < ncol(fData(eset)) && xref > 0) {
      xrefs <- fData(eset)[, xref]
    } else {
      stop("When of length 1, \"xref\" must be the column name of xrefs or the column index")
    }
  } else {
    if(length(xref) != dim(eset)[1]) {
      stop("xref length not equal to the eset feature size")
    } else {
      xrefs <- xref
    }
  }
  
  if(missing(modules))
    modules <- seq_len(ncol(isaRes@genes))

  feature.names <- getFeatureNames(isaRes, modules)
  feature.xrefs <- lapply(feature.names, function(x) xrefs[match(x, featureNames(eset))])
  return(feature.xrefs)
}
