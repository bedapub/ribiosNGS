normByGroup <- function(eset, group, method=c("rma", "mas5"), ...) {
  method <- match.arg(method)
  if(method=="rma") {
    mf <- rma
  } else if (method=="mas5") {
    mf <- mas5
  } else {
    stop("suppoed 'method': rma, mas5\n")
  }
  group <- factor(group)
  nsamples <- length(sampleNames(eset))
  if(nsamples!=length(group))
    stop("eset must be of the same length of group")

  if(method=="rma") { ## check each group has least two samples
    gcounts <- table(group)
    if(any(gcounts<2))
      stop("Following groups have less than 2 samples, therefore rma cannot be run:\n",
           paste(names(gcounts)[gcounts<2], sep=","))
  }
  
  suppressMessages(esets <- tapply(seq(1:nsamples),
                                   group,
                                   function(x) do.call(mf, list(eset[, x]),...)))

  esetsPd <- lapply(esets, pData)
  esetsFd <- fData(eset)
  esetsMat <- lapply(esets, exprs)
  esetsSn <- lapply(esets, sampleNames)

  esetPd <- do.call(rbind, esetsPd)
  esetMat <- do.call(cbind, esetsMat)
  colnames(esetMat) <- rownames(esetPd) <- unlist(esetsSn)
  rownames(esetMat) <- featureNames(eset)
  if(nrow(esetsFd)==nrow(esetMat)) {
    rownames(esetsFd) <- rownames(esetMat)
  } else {
    esetsFd <- data.frame(row.names=featureNames(eset))
  }
  
  rawEset <- new("ExpressionSet",
                 exprs=esetMat,
                 featureData=new("AnnotatedDataFrame", esetsFd),
                 phenoData=new("AnnotatedDataFrame", esetPd),
                 annotation=annotation(eset))
  eset <- rawEset[, match(sampleNames(eset), sampleNames(rawEset))]
  return(eset)
}
