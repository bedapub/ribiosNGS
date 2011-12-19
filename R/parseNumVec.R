parseNumVec <- function(str, expLen=2, failVal=c(5,5), sep=",") {
  ## for cases like 2, 2 (note tht extra blank after the comma)
  str <- paste(str, collapse=sep)

  ## remove quotings if any
  str <- gsub("\"", "", str)
  
  if(length(str)==1) {
    str <- strsplit(str, sep)[[1]]
  }
  str <- str[str!=""]

  isNum <- suppressWarnings(all(!is.na(as.numeric(str))))
  if(!is.null(expLen)) {
    isNum <- isNum && length(str) == expLen
  }
  
  if(isNum) {
    return(as.numeric(str))
  } else {
    return(failVal)
  }
}
