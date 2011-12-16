##------------------------------##
## export and import gct/cls files
##------------------------------##

## Export ExpressionSet into gct/cls files
writeGct <- function(eset, file=stdout(),
                     feature.name.col,
                     feature.desc.col)  {
  prefix <- paste("#1.2", "\n",
                  nrow(eset), "\t", ncol(eset),sep="")
  writeLines(prefix,file)
  
  if(missing(feature.name.col)) {
    fnames <- featureNames(eset)
  } else {
    fnames <- fData(eset)[, feature.name.col]
  }
  if(missing(feature.desc.col)) {
    fdesc <- ""
  } else {
    fdesc <- fData(eset)[, feature.desc.col]
  }
  df <- data.frame(NAME=fnames,
                   Description="")
  df <- cbind(df, exprs(eset))
  suppressWarnings(write.table(df, file=file, append=TRUE, quote=FALSE, sep="\t",
                               row.names=FALSE, col.names=TRUE, na=""))
}
writeCls <- function(eset, file=stdout(), sample.group.col) {
  if(missing(sample.group.col)) {
    stop("Sample groups must be given by the sample.group.col\n")
  }
  cs <- dfFactor(pData(eset), sample.group.col)
  nc <- nlevels(cs)
  str1 <- paste(ncol(eset), nc, 1L, sep=" ")
  str2 <- paste("#", paste(levels(cs), collapse=" "), sep=" ")
  str3 <- paste(as.integer(cs)-1L, collapse=" ")
  writeLines(c(str1, str2, str3),
             con=file)
}
writeGctCls <- function(eset,
                        file.base,
                        feature.name.col,
                        sample.group.col,
                        write.add.fData.file=TRUE,
                        write.add.pData.file=TRUE) {
  gct.file <- paste(file.base, ".gct", sep="")
  cls.file <- paste(file.base, ".cls", sep="")
  writeGct(eset, file=gct.file, feature.name.col=feature.name.col)
  writeCls(eset, file=cls.file, sample.group.col=sample.group.col)
  if(write.add.fData.file) {
    add.fData.file <- paste(file.base, ".add.fData.txt", sep="")
    write.table(fData(eset), file=add.fData.file, sep="\t", col.names=TRUE)
  }
  if(write.add.pData.file) {
    add.pData.file <- paste(file.base, ".add.pData.txt", sep="")
    write.table(pData(eset), file=add.pData.file, sep="\t", col.names=TRUE)
  }
}

## Import ExpressionSet into gct/cls files
## the C version, about 5x faster than the R implementation for the ALL dataset
readGct <- function(gct.file) {
  mat <- read_gct_matrix(gct.file, keep.desc=TRUE)
  desc <- attr(mat, "desc")
  attr(mat, "desc") <- NULL
  
  fnames <- rownames(mat)
  if(any(duplicated(fnames))) {
    warning("Duplicated feature names detected, they will be made unique.\n")
    fnames <- make.unique(fnames)
  }
  res <- new("ExpressionSet",
             exprs=mat,
             featureData=new("AnnotatedDataFrame",
               data.frame(desc=desc, row.names=fnames)),
             phenoData=new("AnnotatedDataFrame",
               data.frame(row.names=colnames(mat))))
  res
}

readCls <- function(cls.file) {
  lns <- readLines(cls.file)
  stopifnot(length(lns)==3)
  indims <- as.integer(strsplit(lns[1L], "\\s")[[1]])
  stopifnot(length(indims)==3L & indims[3L]==1L)
  slevels <- strsplit(lns[2L], "\\s")[[1L]]
  stopifnot(identical(slevels[1], "#") & length(slevels)==indims[2L]+1L)
  slevels <- slevels[-1L]
  fn <- as.integer(strsplit(lns[3], "\\s")[[1]])
  stopifnot(length(fn)==indims[1L])

  sf <- as.factor(fn); levels(sf) <- slevels
  return(sf)
}

readGctCls <- function(file.base,
                       gct.file,
                       cls.file,
                       add.fData.file,
                       add.pData.file) {
  has.file.base <- !missing(file.base)
  has.gct.cls <- !missing(gct.file) && !missing(cls.file)

  if(has.file.base && !has.gct.cls) {
    gct.file <- paste(file.base, ".gct", sep="")
    cls.file <- paste(file.base, ".cls", sep="")
  } else if (has.file.base && has.gct.cls) {
    stop("Either 'file.base' or both 'gct.file' and 'cls.file' should be provided, not all at one time\n")
  } else if (!has.file.base && !has.gct.cls) {
    stop("Provide either 'file.base', or both 'gct.file' and 'cls.file' as parameters\n")
  }
  
  stopifnot(file.exists(gct.file) && file.exists(cls.file))

  eset <- readGct(gct.file)
  cls <- readCls(cls.file)
  pData(eset)$cls <- cls

  if(!missing(add.fData.file)) {
    add.fdata <- readFKtable(add.fData.file, featureNames(eset))
    fData(eset) <- cbind(fData(eset), add.fdata)
  }
  
  if(!missing(add.pData.file)) {
    add.pdata <- readFKtable(add.pData.file, sampleNames(eset))
    pData(eset) <- cbind(pData(eset), add.pdata)
  }

  return(eset)
}
