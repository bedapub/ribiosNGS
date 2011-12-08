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
