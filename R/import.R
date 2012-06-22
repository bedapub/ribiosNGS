## read matrix
readExprsMatrix <- function(x) {
  exp <- read_exprs_matrix(x)
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
                                      chip,
                                      orthologue=FALSE) {
  if(missing(chip)) chip <- ""
  
  pre.scan <- scan(filename, what="character", sep="\n", nmax=200L, quiet=TRUE)
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

  if(require(ribiosAnnotation)) {
    feature.data.frame <- annotateProbesets(feature.names, chip, orthologue=orthologue)
  } else {
    warning("ribiosAnnotation is not available. Features are not annotated")
  }
  colnames(exprs.matrix) <- rownames(pheno.data.frame)
  
  expSet <- new("ExpressionSet",
                exprs=exprs.matrix,
                phenoData=new("AnnotatedDataFrame", pheno.data.frame),
                featureData=new("AnnotatedDataFrame", feature.data.frame))
  annotation(expSet) <- chip
  rm(exprs.matrix, pheno.data.frame, feature.data.frame)
  gc(reset=TRUE)
  return(expSet)
}


## import partek files
partek2ExpressionSet <- function(filename,
                                 chip,
                                 orthologue=FALSE) {
  if(missing(chip)) chip <- ""
    
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
  if(require(ribiosAnnotation)) {
    rf <- annotateProbesets(rownames(raw.exp), chip, orthologue=orthologue)
  } else {
    warning("ribiosAnnotation is not available. Features are not annotated")
  }
  eset <- new("ExpressionSet",
              exprs=raw.exp,
              featureData=new("AnnotatedDataFrame", rf),
              phenoData=new("AnnotatedDataFrame", rp))
  return(eset)
}
