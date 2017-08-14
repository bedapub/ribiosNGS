#' Parse a character string into a numveric vector
#'
#' Numeric vectors can be given as arguments in two ways: (1) separated by
#' blanks or (2) separated by other common separators, such as comma
#' (,). This function parses a string, or a string vector into a numeric
#' vector of expected length. In addition it is failure safe: user can
#' specify the return value in case the parsing was not successful,
#'
#'   The input value mostly comes from return values of the \code{\link{argGet}} function.

#' @param str A character string
#' @param expLen Integer or \code{NULL}, Expected length of the numeric vector.  When set to NULL, the numeric vector can be of variable length.
#' @param failVal If the parsing failed (for example length not correct, or non-numeric values were provided, this value will be returned
#' @param sep Separator in the character string, default ","
#'
#' @seealso \code{\link{argGet}}
#' 
#' @export
#' @examples
#' parseNumVec("3,7,9", expLen=3)
parseNumVec <- function(str, expLen=2, failVal=c(5,5), sep=",") {
  ## for cases like 2, 2 (note tht extra blank after the comma)
  if(is.null(str))
    return(failVal)

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

#' Parse a character string into string vectors
#'
#' This function parses collapsed multiple options into a vector of
#' character strings. Each option is optionally trimmed of leading and tailing empty
#' spaces given by \code{trim}. See examples.
#'
#' @details
#'  In case of multiple separators, they can be given by concatenating
#'  with piple signs, e.g. \code{,|\\t}.
#'  If input string is \code{NULL}, the function returns \code{NULL}. This
#'  can be useful in case the parameter is optional and not specified.
#'
#' @param str A character string to be parsed
#' @param collapse Character(s) used in the character string to concatenate strings
#' @param trim Logical, whether additional spaces should be trimmed
#' @param ... Further parameters passed to \code{\link[ribiosUtils]{trim}} for fine-tuning of trimming
#'
#' @return A vector of character strings
#' @seealso \code{\link{strsplit}}, \code{\link[ribiosUtils]{trim}}
#' @importFrom ribiosUtils trim
#' @export
#' @examples
#' parseStrings("veni, vidi, vici")
#' parseStrings("veni, vidi, vici", trim=FALSE)
#' parseStrings("I came, I saw, I conquered")
#'
#' # options are trimmed
#' parseStrings("a,b,\tc,d\n")
#' # it works also with only one option
#' parseStrings("a")
#' # more than one separators
#' parseStrings("a,b,c;d", collapse=",|;")
parseStrings <- function(str, collapse=",", trim=TRUE, ...) {
  if(is.null(str)) return(NULL)
  res <- strsplit(str, collapse)[[1]]
  if(trim)
    res <- sapply(res, trim, ...)
  return(res)
}


#' Parse key-value pairs from a character string
#'
#'   The function parses parameters in the form of
#'  \code{KEY1=VAL1,KEY2=VAL2,KEY3=VAL3} into \code{data.frame}.
#' 
#'  If input string is \code{NULL}, the function returns \code{NULL}. This
#' can be useful in case the parameter is optional and not specified.
#' @param str Character string
#' @param collapse Collapse character used in the string
#' @param sep Seperator used in the string
#' @param colnames Column names of the returned \code{data.frame}
#' @param trim Logical, whether additional spaces should be trimmed
#' @param ... Further parameters passed to \code{\link[ribiosUtils]{trim}} for fine-tuning of trimming
#' 
#' @export
#' @return A \code{data.frame} containing keys and values
#' @seealso \code{\link{parseStrings}}
#' @examples
#'
#' parsePairs("A=3,B=4,C=5", collapse=",", sep="=")
#' parsePairs("A:3|B:4|C:5", collapse="|", sep=":")
#' 
parsePairs <- function(str, collapse=",", sep="=",
                       colnames=c("key", "value"),
                       trim=TRUE,...) {
  if(is.null(str)) return(NULL)
  strv <- parseStrings(str, collapse=collapse, trim=trim, ...)
  strl <- strsplit(strv, sep)
  res <- data.frame(key=I(sapply(strl, "[", 1L)),
                    value=I(sapply(strl, "[", 2L)))
  colnames(res) <- colnames
  return(res)
}


#' Make a factor
#'
#' @param groups Character strings
#' @param levels Character vector, indicating strings
#' @param make.names Should names be converted to adhere to the rule of variable names in R
#' @param verbose Logical vector
#'
#' @export
#' @examples
#' makeFactor(c("A", "B", "C", "C", "A"), levels=LETTERS[3:1])
#' makeFactor(c("A 1", "B 2", "C 3", "C 3", "A 1"),
#'     levels=c("A 1", "C 3", "B 2"),
#'     make.names=TRUE)
#' makeFactor(c("A 1", "B 2", "C 3", "C 3", "A 1"),
#'     levels=c("A 1", "C 3", "B 2"),
#'     make.names=FALSE)
#' makeFactor(c("A 1", "B 2", "C 3", "C 3", "A 1"),
#'     levels=c("A 1", "C 3", "B 2"),
#'     make.names=FALSE, verbose=TRUE)
makeFactor <- function(groups, levels=NULL, make.names=TRUE, verbose=FALSE) {
  if(missing(levels) || is.null(levels)) {
    if(is.factor(groups)) {
      levels <- levels(groups)
    } else {
      levels <- levels(factor(groups))
    }
  }

  if(!all(groups %in% levels)) {
    missing.groups <- setdiff(groups, levels)
    stop("Following groups were not in levels:", paste(missing.groups, collapse=","),"\n")
  }
  groups <- factor(groups, levels=levels)
  if(make.names) {
    groups.back <- groups
    levels(groups) <- make.unique(make.names(levels(groups)))
    if(!identical(levels(groups.back), levels(groups))) {
      isChanged <- levels(groups.back)!=levels(groups)
      if(verbose) {
        msg <- sprintf("%s->%s",
                       levels(groups.back)[isChanged],
                       levels(groups)[isChanged])
        warning("The following group names has been changed:\n",
                paste(msg, collapse="\n"))
      }
    }
  }
  return(groups)
}


#' Parse a character string into factor
#'
#' @param str A character string giving groups
#' @param rlevels A character string giving levels
#' @param make.names Logical, should names be converted to adhere to the rule of variable names in R
#' @param collapse Character used in \code{relevels} to collapse different levels
#'
#' @export
#' @examples
#' parseFactor("A,B,C,B,A", rlevels="A,B,C")
#'
#' rgroup <- "A,B,C,D,B,C,A,D,B"
#' rlevels <- "D,A,B,C"
#' parseFactor(rgroup, rlevels)
#' 
#' groups <- c("ATest", "Control", "Control", "ATest")
#' levels <- c("Control", "ATest")
#' makeFactor(groups, levels)
#' 
#' # if 'groups' is a factor and 'levels' NULL or missing, its levels are respected
#' groups <- factor(c("B", "C", "A", "D"), levels=c("D","C","A","B"))
#' makeFactor(groups)
#' 
#' \dontrun{
#' groups <- c("ATest", "Control", "Control", "ATest")
#' levels <- c("Control", "ATest", "Unknown")
#' makeFactor(groups, levels)
#' 
#' groups <- c("ATest", "Control", "Control", "ATest", "BTest")
#' levels <- c("Control", "ATest")
#' makeFactor(groups, levels)
#' }
#' 
parseFactor <- function(str, rlevels=NULL, make.names=TRUE, collapse=",") { ## CL=command line
  if(is.null(str))
    stop("raw string of groups cannot be NULL")
  groups <- unname(parseStrings(str, collapse=collapse))
  if(!missing(rlevels) && !is.null(rlevels)) {
    grouplevels <- parseStrings(rlevels, collapse=collapse)
  } else {
    grouplevels <- NULL
  }
  makeFactor(groups, grouplevels, make.names=make.names)
}

## parse files from command line option, which can be (1) a string vector of files, (2) a file listing input files (e.g. pointer file), (3) a directory, or (4) a zip/tar/gz file (determined by suffix). In the later two cases, file patterns can be specified
## in case of compressed files, a temp dir will be created: the user should take care of cleaning up!
isDir <- function(str) file.info(str)$isdir
## TODO: parseFiles is not checked yet!

#' Parse files from command-line options
#'
#' Parse files from command line option, which can be (1) a string vector of files, (2) a file listing input files (e.g. pointer file), (3) a directory, or (4) a zip/tar/gz file (determined by suffix). In the later two cases, file patterns can be specified.
#'
#' @note In case of compressed files, a temp dir will be created: the user should take care of cleaning up!
#'
#' @param str A character string
#' @param sep Seperator used in the string
#' @param pattern Pattern string, if given, only files matching the pattern will be returned
#' @param recursive In cse of directory or compressed files, whether files should be found recursively
#' @param ignore.case In case of directory or compressed files, whether case should be ignored
#'
#' @importFrom ribiosUtils extname
#' @importFrom utils untar unzip
#' @export
parseFiles <- function(str, sep=",", pattern=NULL, recursive=TRUE, ignore.case=TRUE) {
  if(file.exists(str)) { ## a compressed file or a directory
    if(isDir(str)[1]) { ## directory
      selfiles <- dir(str, pattern=pattern, full.names=TRUE,
                      recursive=recursive, ignore.case=ignore.case)
    } else {
      inext <- extname(str, lower.case=TRUE)
      if(!is.na(inext) & inext %in% c("zip", "tar", "gz")) { ## compressed file
        indir <- tempdir()
        if(inext=="zip") {
          unzip(zipfile=str, exdir=indir)
        } else { ## assume that the file is a tar.* file 
          untar(tarfile=str, exdir=indir)
        }
        selfiles <- dir(indir, pattern=pattern, full.names=TRUE,
                        recursive=recursive, ignore.case=ignore.case)
      } else { ## list file
        selfiles <- readLines(str)
      } 
    }
  } else { ## file names concatenated by commas(,)
    selfiles <- parseStrings(str)
  }
  return(selfiles)
}
