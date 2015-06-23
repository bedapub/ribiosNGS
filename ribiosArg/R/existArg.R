existArg <- function(args) {
  argPattern <- paste("^-", args, "$", sep="")

  ## allArgs <- c("R", "-f", "test.R", "-infile", "a.file", "b.file", "-outfile", "o.file", "o2.file", "-value", "2", "3", "-hey", "-hallo")
  ## allArgs <- c("/apps/bi/apps/R/R-2.11.1/bin/exec/R", "-f", "drawHeatmap.R")
  allArgs <- commandArgs(trailingOnly=FALSE)
  
  argInd <- lapply(argPattern, function(x) grep(x, allArgs))
  argIndL <- sapply(argInd, length)
  res <- vector("logical", length(args))
  res[argIndL==1L] <- TRUE
  if(any(argIndL>1)) {
    warning("Multiple values found:", paste(argPattern[argIndL>1], collapse="\t"), "\n")
    res[argIndL>1L] <- TRUE
  }
  names(res) <- args
  return(res)
}
