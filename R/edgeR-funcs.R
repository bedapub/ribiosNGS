edgeR.DGE <- function(path=".",
                      targets.file="Targets.txt",
                      normFactor.method="RLE") {
  target.file.full <- file.path(path,targets.file)
  if(!file.exists(target.file.full))
    stop("target.file not found at ", target.file.full)
  targets <- read.delim(file=target.file.full,
                        stringsAsFactors=FALSE)
  data <- readDGE(targets, path=path, comment.char="#", head=FALSE)
  d <- calcNormFactors(data, method=normFactor.method)
  d <- estimateCommonDisp(d)
  d
}

edgeR.sumCountByGroup <- function(data,
                                  group.colName="group"){
  if(!group.colName %in% colnames(data$sample))
    stop("'group.colName' must be one of the following:\n",
         paste(colnames(data$sample), collapse=","))
  do.call(cbind,
          tapply(1:ncol(data$counts),
                 data$sample[,group.colName],
                 function(x) rowSums(data$counts[,x,drop=FALSE])))
}
