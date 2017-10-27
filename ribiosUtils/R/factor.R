is.named <- function(x) !is.null(names(x))

raiseMessage <- function(msg, condition=c("warning", "error")) {
  condition <- match.arg(condition)
  if(condition=="warning") {
    warning(msg)
  } else if (condition=="error") {
    stop(msg)
  } else {
    print("Should not be here.")
  }
}

checkFactorLevels <- function(factor, levels, 
                              missingLevels=c("pass", "warning", "error"), 
                              unrecognisedLevels=c("warning", "pass", "error")) 
{ 
  missingLevels <- match.arg(missingLevels)
  unrecognisedLevels <- match.arg(unrecognisedLevels)
  flevels <- levels(factor)
  if(missingLevels!="pass") {
    notCovered <- setdiff(flevels, levels)
    if(length(notCovered)>0) {
      msg <- paste("Following levels are not covered by refs:",
                   paste(notCovered, collapse=", "), sep="\n")
      raiseMessage(msg, condition=missingLevels)
    }
  }
  if(unrecognisedLevels!="pass") {
    unrecog <- setdiff(levels, flevels)
    if(length(unrecog)>0) {
      msg <- paste("Following levels are not recognised in x:",
                   paste(unrecog, collapse=", "), sep="\n")
      raiseMessage(msg, condition=unrecognisedLevels)
    }
  }
}

#' Relevel a factor by a named vector
#' 
#' @description Relevel a factor by a named vector. 
#' @param x A factor
#' @param refs A named vector. The names of the vector are all or a subset of levels in the old factor.
#'       And the values are new levels
#' @param missingLevels Actions taken in case existing levels are missing: 'pass', 'warning', or 'error'.
#' @param unrecognisedLevels Actions taken in case unrecognised levels are found: 'pass', 'warning', or 'error'.
#' 
#' @details
#' If names contain character strings other than the levels in the old factor and warning is set to \code{TRUE}, a warning will be raised
#' @examples
#' oldFactor <- factor(c("A", "B", "A", "C", "B"), levels=LETTERS[1:3])
#' factorDict <- c("A"="a", "B"="b", "C"="c")
#' newFactor <- relevelsByNamedVec(oldFactor, factorDict)
#' stopifnot(identical(newFactor, factor(c("a", "b", "a", "c", "b"), levels=c("a", "b", "c"))))
#' ## TODO: test warning and error
relevelsByNamedVec <- function(x, refs, 
                               missingLevels=c("pass", "warning", "error"), 
                               unrecognisedLevels=c("warning", "pass", "error")) {
  stopifnot(is.named(refs))
  stopifnot(is.factor(x))
  xlevels <- levels(x)
  refNames <- names(refs)
  
  checkFactorLevels(x, refNames,
                    missingLevels=missingLevels,
                    unrecognisedLevels=unrecognisedLevels)
  
  commonLevels <- intersect(xlevels, refNames)
  indOld <- match(commonLevels, xlevels)
  indNew <- match(commonLevels, refNames)
  levels(x)[indOld] <- refs[indNew]
  return(x)
}

#' Relevel a factor by an unnamed vector
#' 
#' @description Relevel a factor by a unnamed vector. 
#' @param x A factor
#' @param refs A unnamed vector. The values of the vector are levels of \code{x}.
#' @param missingLevels Actions taken in case existing levels are missing: 'pass', 'warning', or 'error'.
#' @param unrecognisedLevels Actions taken in case unrecognised levels are found: 'pass', 'warning', or 'error'.
#' 
#' @details 
#' If names contain character strings other than the levels in the old factor and warning is set to \code{TRUE}, a warning will be raised
#' @examples
#' oldFactor <- factor(c("A", "B", "A", "C", "B"), levels=LETTERS[1:3])
#' refLevels <- c("B", "C", "A")
#' newFactor <- relevelsByNotNamedVec(oldFactor, refLevels)
#' stopifnot(identical(newFactor, factor(c("A", "B", "A", "C", "B"), levels=c("B", "C", "A"))))
#' ## TODO: test warning and error
#' 
relevelsByNotNamedVec <- function(x, refs, 
                                  missingLevels=c("pass", "warning", "error"), 
                                  unrecognisedLevels=c("warning", "pass", "error")) 
{
  stopifnot(!is.named(refs))
  stopifnot(is.factor(x))
  xlevels <- levels(x)
  
  checkFactorLevels(x, refs,
                    missingLevels=missingLevels,
                    unrecognisedLevels=unrecognisedLevels)
  
  refs <- rev(refs)
  for (i in refs) {
    x <- relevel(x, ref = i)
  }
  return(x)
}

#' Relevel a factor by a named or unnamed vector
#' 
#' @description Relevel a factor by a named or unnamed vector. 
#' 
#' @param x A factor
#' @param refs A named vector or unnamed vector. 
#' @param missingLevels Actions taken in case existing levels are missing: 'pass', 'warning', or 'error'.
#' @param unrecognisedLevels Actions taken in case unrecognised levels are found: 'pass', 'warning', or 'error'.
#' 
#' @details 
#' This function wraps \code{\link{relevelsByNamedVec}} for named vector and \code{\link{relevelsByNotNamedVec}} for not named vectors
#' @seealso \code{\link{relevelsByNamedVec}} and \code{\link{relevelsByNotNamedVec}}
#' @examples
#' oldFactor <- factor(c("A", "B", "A", "C", "B"), levels=LETTERS[1:3])
#' refLevels <- c("B", "C", "A")
#' refDict <- c("A"="a", "B"="b", "C"="c")
#' newFactor <- relevels(oldFactor, refLevels)
#' stopifnot(identical(newFactor, factor(c("A", "B", "A", "C", "B"), levels=c("B", "C", "A"))))
#' newFactor2 <-  relevels(oldFactor, refDict)
#' stopifnot(identical(newFactor2, factor(c("a", "b", "a", "c", "b"), levels=c("a", "b", "c"))))
#' \dontrun{
#' try(relevels(oldFactor, c("A", "B", "C", "D"),  unrecognisedLevels="error"))
#' try(relevels(oldFactor, c("A", "B"), missingLevels="error"))
#' }
relevels <- function(x, refs,
                     missingLevels=c("pass", "warning", "error"), 
                     unrecognisedLevels=c("warning", "pass", "error")) {
  
  missingLevels <- match.arg(missingLevels)
  unrecognisedLevels <- match.arg(unrecognisedLevels)
  
  if(is.named(refs)) {
    res <- relevelsByNamedVec(x, refs, 
                              missingLevels=missingLevels,
                              unrecognisedLevels=unrecognisedLevels)
  } else {
    res <- relevelsByNotNamedVec(x, refs, 
                                 missingLevels=missingLevels,
                                 unrecognisedLevels=unrecognisedLevels)
  }
  return(res)
}


ofactor <- function(x,...) factor(x, levels=unique(as.character(x)),...)

##test.relevels <- function() {
##  cup <- c("HSV","FCBayern","KSC","VfB")
##  teams <- factor(cup)
##  orderTeams <- relevels(teams, cup)
##
##  checkEquals(levels(orderTeams), cup)
##  checkException(relvels(teams, c(cup, "SF")))
##}

cutInterval <- function(x, step=1,
                        labelOption=c("cut.default", "left", "right"),
                        include.lowest=FALSE, right=TRUE, dig.lab=3, ordered_result=FALSE,...) {
  labelOption <- match.arg(labelOption,
                     c("left", "right", "cut.default"))
  x.max <- max(x, na.rm=TRUE)
  x.min <- min(x, na.rm=TRUE)
  cut.up <- ifelse(x.max %% step==0,
                   x.max %/% step, x.max %/%step+1)*step
  cut.low <- ifelse(x.min %/% step==0,
                    0, step * (x.min %/% step))
  cut.scale <- seq(from=cut.low, to=cut.up, by=step)
  labels <- NULL
  if(labelOption=="left") {
    labels <- cut.scale[-length(cut.scale)]
  } else if (labelOption=="right") {
    labels <- cut.scale[-1]
  }
  x.cut <- cut(x, cut.scale,labels=labels,
               include.lowest=include.lowest, 
               right=right, 
               dig.lab=dig.lab, 
               ordered_result=ordered_result, ## default in cut
               ...)
  return(x.cut)
}

refactorNum <- function(x, decreasing=FALSE) {
  x <- factor(as.character(x))
  new.levels <- sort(as.numeric(levels(x)),
                     decreasing=decreasing)
  factor(x, levels=new.levels)
}

#' Convert factor columns in a data.frame into character strings
#' @param df A data.frame
#' @return A data.frame with factor columns coereced into character strings
#' @examples
#' exampleDf <- data.frame(Teams=c("HSV", "FCB", "FCB", "HSV"), 
#'              Player=c("Mueller", "Mueller", "Robben", "Holtby"), 
#'              scores=c(3.5, 1.5, 1.5, 1.0), stringsAsFactors=TRUE)
#' strDf <- dfFactor2Str(exampleDf)
#' stopifnot(identical(strDf[,1], c("HSV", "FCB", "FCB", "HSV")))
#' stopifnot(identical(exampleDf[,1], factor(c("HSV", "FCB", "FCB", "HSV"))))
dfFactor2Str <- function(df) {
  isFactor <- sapply(df, is.factor)
  ind <- which(isFactor)
  if(any(isFactor)) 
    for(i in ind)
      df[,i] <- as.character(df[,i])
  return(df)
}
