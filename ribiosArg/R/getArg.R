#' An R-implementation of getting named aguments
#'
#' This function is out-dated. Please use \code{argparse} instead.
#'
#'   Options are those arguments with a leading minus sign. They can
#' have one or more values following them, which will be taken as the value
#' of the option. If no such values are availble, user could decide how to 
#' interpret the option by setting the \code{onlyArg} parameter. Similarly,
#' missing options can be handled by \code{missingArg}
#'
#' From version 1.0.3 \code{onlyArg} and \code{missingArg} accepts \code{NULL} as inputs.
#' 
#' @param args Character strings, named arguments
#' @param onlyArg Any type, What value should be returned if only the option is available and no value has been provided
#' @param missingArg Any type, What value should be returned if the option is not available
#'
#' @return A list when more than one option were queried; or a vector if only one option was queried.
#' @seealso \code{\link{existArg}}
#' 
#' @export
#'
getArg <- function(args, onlyArg=FALSE, missingArg=FALSE) {
  .Deprecated("argparse", package="ribiosArg")
  argPattern <- paste("^-", args, "$", sep="")

  ## for testing purporses
  ## allArgs <- c("R", "-f", "test-ab.R", "-infile", "a.file", "b.file", "-outfile", "o.file",
  ##              "o2.file", "-value", "2", "3", "-hey", "-hallo")
  ## allArgs <- c("/apps/bi/apps/R/R-2.11.1/bin/exec/R", "-f", "drawHeatmap.R")

  ## add support of NULL: note that assigning a list item to NULL will actually remove that item

  oaNN <- !is.null(onlyArg)
  maNN <- !is.null(missingArg)
  
  allArgs <- commandArgs(trailingOnly=FALSE)

  argAllInd <- grep("^-[^\\.0-9]", allArgs) ## note that negative numbers are excluded
  argNumStart <- grep("^-[\\.0-9]+[^[:punct:]0-9]", allArgs);
  argAllInd <- c(argAllInd, argNumStart)
  
  res <- vector("list", length(argPattern))
  names(res) <- args
  argInd <- lapply(argPattern, function(x) grep(x, allArgs))
  argIndL <- sapply(argInd, length)
  argRightInd <- argIndL==1L
  if(any(argIndL==0)) {
    nof <- which(argIndL==0L)
    for(i in seq(along=nof))
      if(maNN)
        res[[ nof[i] ]] <- missingArg
  }
  if(any(argIndL>1))
    warning("Multiple values found:", paste(argPattern[argIndL>1], collapse="\t"), "\n")

  if(all(!argRightInd)) {
    if(length(args)==1)
      return(res[[1]])
    return(res)
  }
  argStart <- unlist(argInd[argRightInd])+1L
  ## argEnd: either next arg or end of the arg string
  argEndCan <- c(argAllInd, length(allArgs)+1)

  ari <- which(argRightInd)
  for(i in seq(along=argStart)) {
    d <- argEndCan - argStart[i]
    if(any(d==0)) {
      if(oaNN)
        res[[ ari[i] ]] <- onlyArg
    } else if(!any(d>0)) {
      if(maNN)
        res[[ ari[i] ]] <- missingArg
    } else {
      res[[ ari[i] ]] <- allArgs[argStart[i]:(min(d[d>0])+argStart[i]-1L)]
    }
  }
  

  if(length(res)==1)
    res <- res[[1]]
  if(length(res)==0)
    return(FALSE)
  
  return(res)
                
}
