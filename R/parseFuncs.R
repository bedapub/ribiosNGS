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
  if(is.null(str)) return(NULL)
  strv <- parseStrings(str, collapse=collapse, trim=trim, ...)
  strl <- strsplit(strv, sep)
  res <- data.frame(key=I(sapply(strl, "[", 1L)),
                    value=I(sapply(strl, "[", 2L)))
  colnames(res) <- colnames
  return(res)
}

parseStrings <- function(str, collapse=",", trim=TRUE, ...) {
  if(is.null(str)) return(NULL)
  res <- strsplit(str, collapse)[[1]]
  if(trim)
    res <- sapply(res, trim, ...)
  return(res)
  
}

## makeFactor and parseFactor

makeFactor <- function(groups, levels=NULL, make.names=TRUE) {
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
    groups.back <- groups; levels(groups) <- make.names(levels(groups))
    if(!identical(levels(groups.back), levels(groups))) {
      isChanged <- levels(groups.back)!=levels(groups)
      msg <- sprintf("%s->%s",
                     levels(groups.back)[isChanged],
                     levels(groups)[isChanged])
      warning("The following group names has been changed", msg);
    }
  }
  return(groups)
}
parseFactor <- function(rgroups, rlevels=NULL, make.names=TRUE, collapse=",") { ## CL=command line
  if(is.null(rgroups))
    stop("raw string of groups cannot be NULL")
  groups <- unname(parseStrings(rgroups, collapse=collapse))
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
