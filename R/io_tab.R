writeEset <- function(eset,exprs.file,fData.file,pData.file) {
  write.table(exprs(eset),
              exprs.file,
              sep="\t", row.names=TRUE, col.names=TRUE, quote=FALSE)
  if(!missing(fData.file) && !is.null(fData.file))
    write.table(fData(eset),
                fData.file,
                sep="\t", row.names=TRUE, col.names=TRUE)
  if(!missing(pData.file) && !is.null(pData.file))
    write.table(pData(eset),
                pData.file,
                sep="\t", row.names=TRUE, col.names=TRUE)
}

readEset <- function(exprs.file,fData.file,pData.file,
                     sep="\t", header=TRUE, ...) {
  ef <- data.matrix(read.table(exprs.file,
                               check.names=FALSE,
                               sep=sep, header=header, ...))
  if(!missing(fData.file) && !is.null(fData.file)) {
    ff <- readFKtable(fData.file, rownames(ef),
                      sep=sep, header=header,
                      ...)
    fd <- new("AnnotatedDataFrame", ff)
  } else {
    fd <- new("AnnotatedDataFrame",
              data.frame(row.names=rownames(ef))
              )
  }
  if(!missing(pData.file) && !is.null(pData.file)) {
    pf <- readFKtable(pData.file,
                      sep=sep, header=header,
                      colnames(ef),...)
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
