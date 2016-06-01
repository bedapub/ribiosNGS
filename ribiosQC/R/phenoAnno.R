#' @export annoEset
annoEset <- function(eset, file=NULL, strict=FALSE) {
  sampNames <- cel2samplename(sampleNames(eset))
  if(is.null(file)) {
    pData(eset) <- data.frame(row.names=sampNames)
  } else {
    pD <- read.table(file, header=TRUE, comment.char="",
                     sep="\t", check.names=FALSE, row.names=NULL) ## row.names=NULL forces row numbering
    pdNames <- cel2samplename(pD[,1L])
    sampInd <- imatch(sampNames, pdNames) ## sampNames (sample names from input file) must have one or more rows in pD (sample names from sample annotation file) by matching the names.
    if(any(is.na(sampInd))) {
      badSamples <- paste(sampNames[is.na(sampInd)], collapse=",")
      if(strict) {
        qqmsg("Error: following sample names have no phenoData associated:",
              badSamples,
              status=1L)
      } else {
        warning("Warning: following sample names have no phenoData associated:",
                badSamples)
      }
    }
    revInd <- imatch(pdNames, sampNames); hasCel <- !is.na(revInd) ## it is tolerated if the sample annotation file have rows that do not have CEL files
    pd <- pD[hasCel,,drop=FALSE]
    eset <- eset[, revInd[hasCel],drop=FALSE]
    rownames(pd) <- sampNames[revInd[hasCel]]
    pData(eset) <- pd
  }
  return(eset)
}

#' @export getNewProp
getNewProp <- function(eset, ## eset object
                       props, ## existing property (properties) used to build the new property, strings separated by commas
                       assigns, ## property assignment in the form of "SAMPLE1=PROP,SAMPLE2=PROP,..."
                       default="Undefined") ## default property
{
  pd <- pData(eset)
  existProps <- colnames(pd)
  if(!is.null(props)) {
    props <- parseStrings(props)
    if(!all(props %in% existProps))
      qqmsg("The following column(s) cannot be found in the pheno data\n",
            paste(setdiff(props, existProps),collapse=","), "\n",
            "Available properties include:",
            paste(existProps, collapse=","),
            status=1L)
    vals <- apply(pd[, props, drop=FALSE],1L, paste, collapse="_")
  } else if (!is.null(assigns)) {
    gdf <- parsePairs(assigns, collapse=",", sep="=",
                      colnames=c("sample", "group"))
    gmatch <- matchColumn(sampleNames(eset), gdf, "sample")
    if(any(is.na(gmatch$group)))
      qqmsg("Error: following samples have no assignments associated:",
            paste(with(gmatch, sample[is.na(group)]), collapse=","),
            status=1L)
    vals <- gmatch$group
  } else {
    vals <- rep(default, length.out=nrow(pd))
  }
  return(vals)
}
