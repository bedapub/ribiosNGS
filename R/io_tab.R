writeEset <- function(eset,exprs.file,fData.file,pData.file) {
  write.table(exprs(eset),
              exprs.file,
              sep="\t", row.names=TRUE, col.names=TRUE)
  if(!missing(fData.file))
    write.table(fData(eset),
                fData.file,
                sep="\t", row.names=TRUE, col.names=TRUE)
  if(!missing(pData.file))
    write.table(pData(eset),
                pData.file,
                sep="\t", row.names=TRUE, col.names=TRUE)
}

readEset <- function(exprs.file,fData.file,pData.file) {
  ef <- data.matrix(read.table(exprs.file))
  if(!missing(fData.file)) {
    ff <- readFKdata(fData.file, rownames(ef))
    fd <- new("AnnotatedDataFrame", ff)
  } else {
    fd <- new("AnnotatedDataFrame",
              data.frame(row.names=rownames(ef))
              )
  }
  if(!missing(pData.file)) {
    pf <- readFKdata(pData.file, colnames(ef))
    pd <- new("AnnotatedDataFrame", pf)
  } else {
    pd <- new("AnnotatedDataFrame",
              data.frame(row.names=colnames(ef))
              )
  }
  eset <- new("ExpressionSet",
              exprs=ef,
              featureData=fd,
              phenoData=pd)
}
