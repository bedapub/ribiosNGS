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
  ## in case probes do not have the name starting with [0-9]:
  ##    we rely on the line "is_Scalebase" to be the last line of phenotype
  ##    but is_Scalebase is not found either, we stop the function reporting an error (which should be fixed later)
  if(is.na(probe.start)) {
    scale.start <- grep("is_Scalebase", pre.scan)[1L]
    haltifnot(!is.na(scale.start),
              msg="The function can not detect where the expression matrix starts. Nor can it find a line starting with is_Scalebase. Please report the error to the developer")
    probe.start <- scale.start + 1L
  }
  ncols <- length(strsplit(pre.scan[probe.start],"\t")[[1L]])
  
  pheno.last.line <- probe.start - 1L
  pdata <- read.csv(filename, sep="\t", nrows=pheno.last.line-1L, row.names=1L)
  pheno.data.frame <- data.frame(t(pdata))

  rm(pdata, pre.scan, probe.start)
  
  exprs.matrix <- read.table(filename, skip=pheno.last.line,
                             row.names=1L,
                             colClasses=c("character", rep("numeric", ncols-2L)),
                             sep="\t", comment.char="")
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
  
  raw <- read.csv(filename, sep="\t")
  rawt <- t(raw)
  rm(raw)

  probeFmt <- "^ILMN"
  probeLns <- grep(probeFmt, rownames(rawt))
  if(length(probeLns)==0)
    stop("No probes found: currently only supporting Illumina data\n")
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
