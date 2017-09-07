sortByDimnames <- function(x,row.decreasing=FALSE, col.decreasing=FALSE) {
  x <- x[order(rownames(x), decreasing=row.decreasing),]
  x <- x[, order(colnames(x), decreasing=col.decreasing)]
  return(x)
}

asNumMatrix <- function(x) {
  mat <- apply(x, 2, as.numeric)
  dimnames(mat) <- dimnames(x)
  return(mat)
}
atofMatrix <- function(data.frame) {
  .Deprecated("asNumMatrix",
              package="ribiosUtils")
  asNumMatrix(data.frame)
}
stringDataFrame2numericMatrix <- function(data.frame) {
  .Deprecated("asNumMatrix",
              package="ribiosUtils")
  asNumMatrix(data.frame)
}

putColsFirst <- function(data.frame, columns) {
  stopifnot(all(columns %in% colnames(data.frame)))
  stopifnot(is.data.frame(data.frame) | is.matrix(data.frame))
  data.frame <- data.frame[,c(columns,
                              setdiff(colnames(data.frame), columns))]
  return(data.frame)
}

removeColumns <- function(data.frame, columns, drop=FALSE) {
  col.names <- colnames(data.frame)
  if(!any(columns %in% col.names)) {
    warning("data.frame does not contain following columns:",
         setdiff(columns, col.names))
  }
  data.frame <- data.frame[,setdiff(col.names, columns), drop=drop]
  return(data.frame)
}

## change column names
replaceByMatch <- function(vector, old.items, new.items) {
  stopifnot(all(old.items %in% vector))
  stopifnot(length(old.items)==length(new.items))
  vector.backup <- vector
  for(i in seq(along=old.items)) {
    vector[vector.backup == old.items[i]] <- new.items[i]
  }
  return(vector)
}
replaceColumnName <- function(data.frame, old.names, new.names) {
  col.names <- colnames(data.frame)
  new.col.names <- replaceByMatch(col.names, old.names, new.names)
  colnames(data.frame) <- new.col.names
  return(data.frame)
}
replaceColumnNames <- function(...) {
    .Deprecated("replaceColumnName",
                package="ribiosUtils")
    replaceColumnName(...)
}
      
sortByCol <- function (data.frame, columns,
                       na.last = TRUE,
                       decreasing = TRUE,
                       orderAsAttr=FALSE)  {
    isMatInput <- is.matrix(data.frame)
    if(isMatInput) {
        data.frame <- data.frame(data.frame, check.names=FALSE, check.rows=FALSE, stringsAsFactors=FALSE)
    }
    if(all(is.character(columns))) {
        stopifnot(all(columns %in% colnames(data.frame)))
    } else if (!all(is.numeric(columns)) && !all(is.logical(columns))) {
        stop("'columns' must be one of the following data types: chracters, numeric and logical\n")
    }
    
    subdf <- data.frame[,columns,drop=FALSE]
    local.order <- function(...) order(..., na.last=na.last,decreasing=decreasing)
    ord <- do.call(local.order, subdf) ## see example(order)
    res <- data.frame[ord,,drop=FALSE]
    if(orderAsAttr)
        attr(res, "order") <- ord
    if(isMatInput) {
        res <- as.matrix(res)
    }
    return(res)
}

dfFactor <- function(df, sample.group) {
  if(!is.data.frame(df))
    stop("The function takes a data.frame as input. Don't pass the ExpressionSet object\n")
  if(length(sample.group)==1 & is.character(sample.group)) {
    stopifnot(sample.group %in% colnames(df))
    fac <- df[, sample.group]
  } else if (length(sample.group)==1 & is.numeric(sample.group)) {
    sample.group <- as.integer(sample.group)
    stopifnot(sample.group>=1L & sample.group <= ncol(df))
    fac <- df[, sample.group]
  } else {
    stopifnot(length(sample.group) == nrow(df))
    fac <- sample.group
  }
  if(!is.factor(fac)) fac <- factor(fac)
  return(fac)
}

## match column names

#' Match a given vector to column names of a data.frame or matrix
#'
#' @param data.frame.cols column names of a data.frame. One can also provide a data.frame, which may however cause worse performance since the data.frame is copied
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#'
#' @examples
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' matchColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' matchColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' ## NA will be returned in this case if ignore.case is set to FALSE
#' matchColumnName(myTestDf, myFavTeamsCase, ignore.case=FALSE)
matchColumnName <- function(data.frame.cols, reqCols, ignore.case=FALSE) {
    if(is.data.frame(data.frame.cols))
        data.frame.cols <- colnames(data.frame.cols)
    if(ignore.case) {
        lowInputCol <- tolower(data.frame.cols)
        lowCol <- tolower(reqCols)
        res <- match(lowCol, lowInputCol)
    } else {
        res <- match(reqCols, data.frame.cols)
    }
    return(res)
}

#' Assert whether the required column names exist
#'
#' @param data.frame.cols column names of a data.frame. One can also provide a data.frame, which may however cause worse performance since the data.frame is copied
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#'
#' @details The function calls \code{\link{matchColumnName}} internally to match the column names.
#' @return If all required column names are present, their indices are returned *invisibly*. Otherwise an error message is printed.
#'
#' @examples
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' assertColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' assertColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' \dontrun{assertColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)}
assertColumnName <- function(data.frame.cols, reqCols, ignore.case=FALSE) {
    matchRes <- matchColumnName(data.frame.cols, reqCols, ignore.case=ignore.case)
    if(any(is.na(matchRes))) {
        moreThanOne <- sum(is.na(matchRes))
        stop("Following column%s not found: %s",
             ifelse(moreThanOne, "s were", " was"),
             paste(reqCols[is.na(matchRes)], collapse=","))
    }
    return(invisible(matchRes))
}

#' Subset a data.frame by column name, allowing differences in cases
#'
#' @param data.frame A data.frame object
#' @param reqCols required columns
#' @param ignore.case logical, whether the case is considered
#'
#' @details The function calls \code{\link{assertColumnName}} internally to match the column names.
#' @return If all required column names are present, the data.frame object will be subset to include only these columns and the result data.frame is returned. Otherwise an error message is printed.
#'
#' @examples
#' myTestDf <- data.frame(HBV=1:3, VFB=0:2, BVB=4:6, FCB=2:4)
#' myFavTeams <- c("HBV", "BVB")
#' subsetByColumnName(myTestDf, myFavTeams)
#' myFavTeamsCase <- c("hbv", "bVb")
#' subsetByColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)
#' \dontrun{subsetByColumnName(myTestDf, myFavTeamsCase, ignore.case=TRUE)}
subsetByColumnName <- function(data.frame, reqCols, ignore.case=FALSE) {
    ind <- assertColumnName(data.frame, reqCols, ignore.case=ignore.case)
    res <- data.frame[,ind]
    colnames(res) <- reqCols
    return(res)
}

## variable columns
isVarCol <- function(df) return(apply(df, 2L, ulen) > 1)
isInvarCol <- function(df) !isVarCol(df)
removeInvarCol <- function(df) df[,isVarCol(df), drop=FALSE]

#' Transform a list of character strings into a data.frame
#' @param list A list of character strings
#' @param names Values in the 'Name' column of the result, used if the input list has no names
#' @param col.names Column names of the \code{data.frame}
#' @examples
#' myList <- list(HSV=c("Mueller", "Papadopoulos", "Wood"), FCB=c("Lewandowski", "Robben", "Hummels"),
#'                BVB=c("Reus", "Goetze", "Kagawa"))
#' list2df(myList, col.names=c("Club", "Player"))
list2df <- function(list, names=NULL, col.names=c("Name", "Item")) {
  if(is.null(names))
    names <- names(list)
  if(is.null(names))
    stop("Parameter 'names' cannot be NULL if the list has NULL names")
  res <- data.frame(Name=rep(names, sapply(list, length)),
             Item=unlist(list), row.names=NULL)
  colnames(res) <- col.names
  return(res)
}
