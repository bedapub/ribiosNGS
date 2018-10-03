countTokens <- function(str, split="\t",...) {
  str <- as.character(str)
  sapply(strsplit(str, split, ...), length)
}
nField <- function(str, split="\t",...) {
  .Deprecated("countTokens", package="ribiosUtils")
  countTokens(str=str, split=split,...)
}


strtoken <- function(x, split, index, ...) {
  x <- as.character(x)
  n <- length(x)
  s <- strsplit(x, split = split, ...)
  nc <- unlist(lapply(s, length))
  out <- matrix("", n, max(nc))
  for (i in 1:n) {
    if (nc[i]) 
      out[i, 1:nc[i]] <- s[[i]]
  }
  if(!missing(index) && !is.null(index))
    out <- out[,index]
  out
}


#' Shorten strings to a given number of characters
#' @param str A vector of strings
#' @param nchar The maximal number of characters to keep
#' 
#' Strings with more characters than \code{nchar} will be shortened.
#' 
#' @note 
#' \code{NA} will be kept as they are
#' 
#' @examples 
#' inputStrs <- c("abc", "abcd", "abcde", NA)
#' shortenStr(inputStrs, nchar=4)
#' ## expected outcome: abc, abcd, abcd..., NA
shortenStr <- function(str, nchar=8) {
  stopifnot(nchar>0)
  overLen <- !is.na(str) & nchar(str) > nchar
  res <- str
  res[overLen] <- paste(substr(str[overLen], 1, nchar), "...", sep="")
  return(res)
}

#' Shorten strings to strings with a fix width of characters
#' @param str A vector of strings
#' @param nchar The fixed with
#' 
#' Strings with more or less characters than \code{nchar} are either shortened or filled (with spaces)
#'
#' @note 
#' \code{NA} will be converted to characters and the same fixed width will be applied. The behavior is different from \code{\link{shortenStr}}, where \code{NA} is kept as it is.
#' 
#' @seealso \code{\link{shortenStr}}
#'
#' @examples 
#' inputStrs <- c("abc", "abcd", "abcde", "abcdefg", "NA", NA) 
#' outputStrs <- fixWidthStr(inputStrs, nchar=4)
#' stopifnot(all(nchar(outputStrs)==4))
fixWidthStr <- function(str, nchar=8, align=c("left", "right")) {
  align <- match.arg(align)
  str[is.na(str)] <- "NA"
  overW <- nchar(str)>nchar
  underW <- nchar(str)<nchar
  res <- str
  if(any(overW)) {
    res[overW] <- sapply(str[overW], function(longstr) {
      shortenStr(longstr, pmax(nchar-3L, 0)) ### three dots ... has length 0f 3
    })
  }
  if(any(underW)) {
    res[underW] <- sapply(str[underW], function(shortstr) {
      emps <- paste(rep(" ", nchar-nchar(shortstr)),collapse="")
      if(align=="left") {
        str <- paste(shortstr, emps, sep="")
      } else {
        str <- paste(emps, shortstr, sep="")
      }
      return(str)
    })
  }
  return(res)
}


