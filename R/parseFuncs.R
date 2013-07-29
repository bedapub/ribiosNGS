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

parsePairs <- function(str, collapse=",", sep="=",
                       colnames=c("key", "value"),
                       trim=TRUE,...) {
  strv <- parseStrings(str, collapse=collapse, trim=trim, ...)
  strl <- strsplit(strv, sep)
  res <- data.frame(key=I(sapply(strl, "[", 1L)),
                    value=I(sapply(strl, "[", 2L)))
  colnames(res) <- colnames
  return(res)
}

parseStrings <- function(str, collapse=",", trim=TRUE, ...) {
  res <- strsplit(str, collapse)[[1]]
  if(trim)
    res <- sapply(res, trim, ...)
  return(res)
  
}
