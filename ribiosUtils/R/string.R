#' Count tokens by splitting strings
#' 
#' 
#' @aliases countTokens nField
#' @param str A character string vector
#' @param split Character used to split the strings
#' @param \dots Other parameters passed to the \code{strsplit} function
#' @return Integer vector: count of tokens in the strings
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{strsplit}} to split strings, or a convenient wrapper
#' \code{\link{strtoken}} in this package.
#' @examples
#' 
#' myStrings <- c("HSV\t1887\tFavorite", "FCB\t1900", "FCK\t1948")
#' countTokens(myStrings)
#' 
#' ## the function deals with factors as well
#' countTokens(factor(myStrings))
#' 
#' @export countTokens
countTokens <- function(str, split="\t",...) {
  str <- as.character(str)
  sapply(strsplit(str, split, ...), length)
}

#' @export nField
nField <- function(str, split="\t",...) {
  .Deprecated("countTokens", package="ribiosUtils")
  countTokens(str=str, split=split,...)
}






#' Tokenize strings by character
#' 
#' Tokenize strings by character in a similar way as the \code{strsplit}
#' function in the \code{base} package. The function can return a matrix of
#' tokenized items when \code{index} is missing. If \code{index} is given,
#' tokenized items in the selected position(s) are returned. See examples.
#' 
#' 
#' @param x A vector of character strings; non-character vectors are cast into
#' characters.
#' @param split A character to split the strings.
#' @param index Numeric vector indicating which fields should be returned; if
#' missing or set to \code{NULL}, a matrix containing all fields are returned.
#' @param \dots Other parameters passed to \code{\link{strsplit}}
#' @return A matrix if \code{index} is missing, \code{NULL}, or contains more
#' than one integer indices; otherwise a character vector.
#' @author Jitao David Zhang <jitao_david.zhang@@roche.com>
#' @seealso \code{\link{strsplit}}
#' @references The main body of the function is modified from the
#' \code{strsplit2} function in the \code{limma} package.
#' @examples
#' 
#' myStr <- c("HSV\t1887", "FCB\t1900", "FCK\t1948")
#' strsplit(myStr, "\t")
#' 
#' strtoken(myStr, "\t")
#' strtoken(myStr, "\t", index=1L)
#' strtoken(myStr, "\t", index=2L)
#' 
#' myFac <- factor(myStr)
#' ## do not run
#' \dontrun{
#'   strsplit(myFac, "\t")
#' }
#' strtoken(myFac, "\t")
#' strtoken(myFac, "\t", index=1L)
#' 
#' @export strtoken
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
#' 
#' 
#' @param str A vector of strings
#' @param nchar The maximal number of characters to keep
#' 
#' Strings with more characters than \code{nchar} will be shortened.
#' @note \code{NA} will be kept as they are
#' @examples
#' 
#' inputStrs <- c("abc", "abcd", "abcde", NA)
#' shortenStr(inputStrs, nchar=4)
#' ## expected outcome: abc, abcd, abcd..., NA
#' 
#' @export shortenStr
shortenStr <- function(str, nchar=8) {
  stopifnot(nchar>0)
  overLen <- !is.na(str) & nchar(str) > nchar
  res <- str
  res[overLen] <- paste(substr(str[overLen], 1, nchar), "...", sep="")
  return(res)
}

#' Shorten strings to strings with a fix width of characters
#' 
#' 
#' @param str A vector of strings
#' @param nchar The fixed with
#' @param align Character, how to align
#' Strings with more or fewer characters than \code{nchar} are either shortened
#' or filled (with spaces)
#' @note \code{NA} will be converted to characters and the same fixed width
#' will be applied. The behavior is different from \code{\link{shortenStr}},
#' where \code{NA} is kept as it is.
#' @seealso \code{\link{shortenStr}}
#' @examples
#' 
#' inputStrs <- c("abc", "abcd", "abcde", "abcdefg", "NA", NA) 
#' outputStrs <- fixWidthStr(inputStrs, nchar=4)
#' stopifnot(all(nchar(outputStrs)==4))
#' 
#' @export fixWidthStr
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


