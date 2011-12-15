## read matrix
readExprsMatrix <- function(x) {
  x.first <- readLines(con=x, n=5L, warn=FALSE)
  if(grepl("\\#1.2", x.first[1])) {
    return(readGct(x)) ## GCT file allowed
  } else if(any(grepl("\t", x.first))) { ## tab delimited file
    df <- read.table(x, sep="\t", row.names=1L, header=TRUE, check.names=FALSE)
  } else { ## space delimited file
    df <- read.table(x, sep="", check.names=FALSE)
  }
  exp <- data.matrix(df)
  res <- new("ExpressionSet",
             exprs=exp,
             phenoData=new("AnnotatedDataFrame",
               data.frame(row.names=colnames(exp))),
             featureData=new("AnnotatedDataFrame",
               data.frame(row.names=rownames(exp))))
  return(res)
}

## import ChipFetcher output files as ExpressionSet
ChipFetcher2ExpressionSet <- function(filename,
                                      annotation="hgu133a") {
  pre.scan <- scan(filename, what="character", sep="\n", nmax=200L)
  probe.start <- grep("^[0-9]", pre.scan)[1L]
  ncols <- length(strsplit(pre.scan[probe.start],"\t")[[1L]])
  
  pheno.last.line <- probe.start - 1L
  pdata <- read.csv(filename, sep="\t", nrows=pheno.last.line-1L, row.names=1L)
  pheno.data.frame <- data.frame(t(pdata))

  rm(pdata, pre.scan, probe.start)
  
  exprs.matrix <- read.table(filename, skip=pheno.last.line,
                             row.names=1L,
                             colClasses=c("character", rep("numeric", ncols-2L)),
                             sep="\t")
  exprs.matrix <- data.matrix(exprs.matrix)
  feature.names <- rownames(exprs.matrix)
  
  feature.data.frame <- annotateByChipname(annotation, feature.names)
  colnames(exprs.matrix) <- rownames(pheno.data.frame)
  
  expSet <- new("ExpressionSet",
                exprs=exprs.matrix,
                phenoData=new("AnnotatedDataFrame", pheno.data.frame),
                featureData=new("AnnotatedDataFrame", feature.data.frame))
  annotation(expSet) <- annotation
  rm(exprs.matrix, pheno.data.frame, feature.data.frame)
  gc(reset=TRUE)
  return(expSet)
}


## import partek files: TODO: the annotation is still done by biocAnnotation::annotateByChipname, should switch to ribiosAnnotation!
partek2ExpressionSet <- function(filename,
                                 annotation="illuminaHumanv3.db") {
  if(annotation == "illuminaHumanv3.db") {
    probeFmt <- "^ILMN"
  } else {
    stop("this function only supports illuminaHumanv3.db so far. Please contact support if other type should be needed")
  }
  
  raw <- read.csv(filename, sep="\t")
  rawt <- t(raw)
  rm(raw)
  probeLns <- grep(probeFmt, rownames(rawt))
  if(length(probeLns)==0)
    stop("No probes found\n")
  probeStart <- min(probeLns)
  
  rp <- data.frame(t(rawt[1:probeStart-1L,]))
  raw.char <- rawt[probeStart:nrow(rawt),]
  raw.exp <- matrix(as.numeric(raw.char),
                    nrow=nrow(raw.char), ncol=ncol(raw.char), dimnames=dimnames(raw.char))
  rf <- annotateByChipname(annotation, rownames(raw.exp))
  eset <- new("ExpressionSet",
              exprs=raw.exp,
              featureData=new("AnnotatedDataFrame", rf),
              phenoData=new("AnnotatedDataFrame", rp))
  return(eset)
}
